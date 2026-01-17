use dashmap::DashMap;
use std::sync::Arc;

use super::Document;

#[derive(Debug, Default)]
pub struct DocumentStore {
    documents: DashMap<String, Document>,
}

#[allow(dead_code)]
impl DocumentStore {
    pub fn new() -> Self {
        Self {
            documents: DashMap::new(),
        }
    }

    pub fn open(&self, uri: String, version: i32, content: String) {
        let doc = Document::new(uri.clone(), version, content);
        self.documents.insert(uri, doc);
    }

    pub fn update(&self, uri: &str, version: i32, content: String) -> bool {
        if let Some(mut doc) = self.documents.get_mut(uri) {
            doc.update(version, content);
            true
        } else {
            false
        }
    }

    pub fn close(&self, uri: &str) -> Option<Document> {
        self.documents.remove(uri).map(|(_, doc)| doc)
    }

    pub fn get(&self, uri: &str) -> Option<dashmap::mapref::one::Ref<'_, String, Document>> {
        self.documents.get(uri)
    }

    pub fn get_mut(&self, uri: &str) -> Option<dashmap::mapref::one::RefMut<'_, String, Document>> {
        self.documents.get_mut(uri)
    }

    pub fn contains(&self, uri: &str) -> bool {
        self.documents.contains_key(uri)
    }

    pub fn uris(&self) -> Vec<String> {
        self.documents.iter().map(|r| r.key().clone()).collect()
    }

    pub fn len(&self) -> usize {
        self.documents.len()
    }

    pub fn is_empty(&self) -> bool {
        self.documents.is_empty()
    }

    pub fn with_document<F, R>(&self, uri: &str, f: F) -> Option<R>
    where
        F: FnOnce(&Document) -> R,
    {
        self.documents.get(uri).map(|doc| f(&doc))
    }

    pub fn with_document_mut<F, R>(&self, uri: &str, f: F) -> Option<R>
    where
        F: FnOnce(&mut Document) -> R,
    {
        self.documents.get_mut(uri).map(|mut doc| f(&mut doc))
    }
}

pub type SharedDocumentStore = Arc<DocumentStore>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn document_store_basic() {
        let store = DocumentStore::new();

        store.open("file:///test.wit".into(), 1, "interface foo {}".into());
        assert!(store.contains("file:///test.wit"));
        assert_eq!(store.len(), 1);

        store.update("file:///test.wit", 2, "interface bar {}".into());
        assert!(store.contains("file:///test.wit"));

        let doc = store.close("file:///test.wit");
        assert!(doc.is_some());
        assert!(!store.contains("file:///test.wit"));
        assert!(store.is_empty());
    }

    #[test]
    fn document_store_with_document() {
        let store = DocumentStore::new();
        store.open("file:///test.wit".into(), 1, "hello".into());

        let len = store.with_document("file:///test.wit", |doc| doc.content.len());
        assert_eq!(len, Some(5));

        let missing = store.with_document("file:///missing.wit", |doc| doc.content.len());
        assert_eq!(missing, None);
    }
}
