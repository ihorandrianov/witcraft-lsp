import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    Executable,
} from 'vscode-languageclient/node';
import * as crypto from 'node:crypto';
import * as fs from 'node:fs';
import type { IncomingMessage } from 'node:http';
import * as path from 'node:path';
import * as https from 'node:https';
import { pipeline } from 'node:stream/promises';
import * as tar from 'tar';
import * as yauzl from 'yauzl';
import type { Entry, ZipFile } from 'yauzl';

const REPO = 'ihorandrianov/witcraft-lsp';
const DEFAULT_CHANNEL = 'latest';
const USER_AGENT = 'witcraft-lsp-vscode';

type ReleaseAsset = {
    name: string;
    browser_download_url: string;
};

type ReleaseInfo = {
    tag_name: string;
    assets: ReleaseAsset[];
};

let client: LanguageClient | undefined;

export async function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('wit-lsp');
    let serverPath: string;
    try {
        serverPath = await resolveServerPath(context, config);
    } catch (error) {
        const message = error instanceof Error ? error.message : String(error);
        vscode.window.showErrorMessage(`Failed to start witcraft-lsp: ${message}`);
        throw error;
    }

    const serverExecutable: Executable = {
        command: serverPath,
        args: [],
    };

    const serverOptions: ServerOptions = {
        run: serverExecutable,
        debug: serverExecutable,
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'wit' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.wit'),
        },
    };

    client = new LanguageClient(
        'wit-lsp',
        'WIT Language Server',
        serverOptions,
        clientOptions
    );

    await client.start();
}

export async function deactivate(): Promise<void> {
    if (client) {
        await client.stop();
    }
}

async function resolveServerPath(
    context: vscode.ExtensionContext,
    config: vscode.WorkspaceConfiguration
): Promise<string> {
    const explicitPath = config.get<string>('server.path')?.trim();
    if (explicitPath) {
        return explicitPath;
    }

    const versionOverride = config.get<string>('server.version')?.trim();
    const channel = config.get<string>('server.releaseChannel')?.trim() || DEFAULT_CHANNEL;
    const releaseRef = versionOverride ? normalizeTag(versionOverride) : channel;
    const releaseInfo = await fetchReleaseInfo(releaseRef, !versionOverride);
    const tag = releaseInfo.tag_name;

    const platform = resolvePlatform();
    const arch = resolveArch();
    const assetName = buildAssetName(tag, platform, arch);
    const asset = releaseInfo.assets.find((item) => item.name === assetName);
    if (!asset) {
        throw new Error(`No release asset named ${assetName} found in ${tag}.`);
    }

    const storageRoot = context.globalStorageUri.fsPath;
    const installDir = path.join(storageRoot, 'server', tag);
    await fs.promises.mkdir(installDir, { recursive: true });

    const binaryName = platform === 'windows' ? 'witcraft-lsp.exe' : 'witcraft-lsp';
    const binaryPath = path.join(installDir, binaryName);
    if (await fileExists(binaryPath)) {
        return binaryPath;
    }

    const archivePath = path.join(installDir, assetName);
    await downloadFile(asset.browser_download_url, archivePath);

    const checksumMap = await fetchChecksums(releaseInfo, tag);
    const expectedChecksum = checksumMap.get(assetName);
    if (!expectedChecksum) {
        throw new Error(`No checksum found for ${assetName} in ${tag}.`);
    }

    const actualChecksum = await sha256File(archivePath);
    if (actualChecksum !== expectedChecksum) {
        throw new Error(
            `Checksum mismatch for ${assetName}. Expected ${expectedChecksum} but got ${actualChecksum}.`
        );
    }

    await extractArchive(archivePath, installDir, platform === 'windows');
    if (platform !== 'windows') {
        await fs.promises.chmod(binaryPath, 0o755);
    }

    return binaryPath;
}

function normalizeTag(version: string): string {
    if (version.startsWith('v')) {
        return version;
    }

    return `v${version}`;
}

function resolvePlatform(): 'macos' | 'linux' | 'windows' {
    switch (process.platform) {
        case 'darwin':
            return 'macos';
        case 'linux':
            return 'linux';
        case 'win32':
            return 'windows';
        default:
            throw new Error(`Unsupported platform: ${process.platform}`);
    }
}

function resolveArch(): 'x64' | 'arm64' {
    switch (process.arch) {
        case 'x64':
            return 'x64';
        case 'arm64':
            return 'arm64';
        default:
            throw new Error(`Unsupported architecture: ${process.arch}`);
    }
}

function buildAssetName(tag: string, platform: string, arch: string): string {
    const extension = platform === 'windows' ? 'zip' : 'tar.gz';
    return `witcraft-lsp-${tag}-${platform}-${arch}.${extension}`;
}

async function fetchReleaseInfo(releaseRef: string, useLatest: boolean): Promise<ReleaseInfo> {
    const url = useLatest
        ? `https://api.github.com/repos/${REPO}/releases/latest`
        : `https://api.github.com/repos/${REPO}/releases/tags/${releaseRef}`;

    return await fetchJson<ReleaseInfo>(url);
}

async function fetchChecksums(releaseInfo: ReleaseInfo, tag: string): Promise<Map<string, string>> {
    const checksumAsset = releaseInfo.assets.find((item) => item.name === 'SHA256SUMS');
    if (!checksumAsset) {
        throw new Error(`No SHA256SUMS asset found in ${tag}.`);
    }

    const content = await downloadText(checksumAsset.browser_download_url);
    const entries = new Map<string, string>();
    for (const line of content.split('\n')) {
        const trimmed = line.trim();
        if (!trimmed) {
            continue;
        }

        const parts = trimmed.split(/\s+/);
        if (parts.length < 2) {
            continue;
        }

        entries.set(parts[1], parts[0]);
    }

    return entries;
}

async function fetchJson<T>(url: string): Promise<T> {
    const buffer = await downloadBuffer(url, {
        Accept: 'application/vnd.github+json',
    });
    return JSON.parse(buffer.toString('utf8')) as T;
}

async function downloadText(url: string): Promise<string> {
    const buffer = await downloadBuffer(url);
    return buffer.toString('utf8');
}

async function downloadFile(url: string, destination: string): Promise<void> {
    const buffer = await downloadBuffer(url);
    await fs.promises.writeFile(destination, buffer);
}

async function downloadBuffer(url: string, headers?: Record<string, string>): Promise<Buffer> {
    const response = await requestWithRedirect(url, headers);
    if (response.statusCode !== 200) {
        throw new Error(`Download failed for ${url} with status ${response.statusCode}.`);
    }

    const chunks: Buffer[] = [];
    response.on('data', (chunk: Buffer) => {
        chunks.push(chunk as Buffer);
    });

    await new Promise<void>((resolve, reject) => {
        response.on('end', resolve);
        response.on('error', reject);
    });

    return Buffer.concat(chunks);
}

async function requestWithRedirect(
    url: string,
    headers?: Record<string, string>,
    redirectCount = 0
): Promise<IncomingMessage> {
    if (redirectCount > 5) {
        throw new Error(`Too many redirects while fetching ${url}.`);
    }

    return await new Promise<IncomingMessage>((resolve, reject) => {
        const request = https.get(
            url,
            {
                headers: {
                    'User-Agent': USER_AGENT,
                    ...headers,
                },
            },
            (response) => {
                const statusCode = response.statusCode ?? 0;
                const location = response.headers.location;
                if (statusCode >= 300 && statusCode < 400 && location) {
                    response.resume();
                    requestWithRedirect(location, headers, redirectCount + 1)
                        .then(resolve)
                        .catch(reject);
                    return;
                }

                resolve(response);
            }
        );

        request.on('error', reject);
    });
}

async function sha256File(filePath: string): Promise<string> {
    const hash = crypto.createHash('sha256');
    const stream = fs.createReadStream(filePath);
    await pipeline(stream, hash);
    return hash.digest('hex');
}

async function extractArchive(
    archivePath: string,
    destination: string,
    isWindows: boolean
): Promise<void> {
    if (isWindows) {
        await extractZip(archivePath, destination);
        return;
    }

    await tar.x({
        file: archivePath,
        cwd: destination,
        strict: true,
        filter: (entryPath: string) => {
            resolveArchivePath(destination, entryPath);
            return true;
        },
    });
}

async function extractZip(archivePath: string, destination: string): Promise<void> {
    await new Promise<void>((resolve, reject) => {
        yauzl.open(archivePath, { lazyEntries: true }, (error: Error | null, zipfile?: ZipFile) => {
            if (error || !zipfile) {
                reject(error ?? new Error('Failed to open zip archive.'));
                return;
            }

            zipfile.readEntry();
            zipfile.on('entry', (entry: Entry) => {
                if (/\/$/.test(entry.fileName)) {
                    zipfile.readEntry();
                    return;
                }

                let targetPath: string;
                try {
                    targetPath = resolveArchivePath(destination, entry.fileName);
                } catch (error) {
                    reject(error);
                    return;
                }
                fs.promises
                    .mkdir(path.dirname(targetPath), { recursive: true })
                    .then(() => {
                        zipfile.openReadStream(
                            entry,
                            (streamError: Error | null, readStream?: NodeJS.ReadableStream) => {
                            if (streamError || !readStream) {
                                reject(streamError ?? new Error('Failed to read zip entry.'));
                                return;
                            }

                            const writeStream = fs.createWriteStream(targetPath);
                            readStream.pipe(writeStream);
                            writeStream.on('close', () => {
                                zipfile.readEntry();
                            });
                            writeStream.on('error', reject);
                            }
                        );
                    })
                    .catch(reject);
            });

            zipfile.on('end', resolve);
            zipfile.on('error', reject);
        });
    });
}

function resolveArchivePath(destination: string, archivePath: string): string {
    const destinationRoot = path.resolve(destination);
    const candidate = path.resolve(destinationRoot, archivePath);
    const destinationPrefix = destinationRoot.endsWith(path.sep)
        ? destinationRoot
        : `${destinationRoot}${path.sep}`;

    if (candidate === destinationRoot || candidate.startsWith(destinationPrefix)) {
        return candidate;
    }

    throw new Error(`Unsafe archive entry path: ${archivePath}`);
}

async function fileExists(filePath: string): Promise<boolean> {
    try {
        await fs.promises.access(filePath, fs.constants.F_OK);
        return true;
    } catch {
        return false;
    }
}
