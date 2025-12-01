// ZynML Example: Vector Similarity Search
// Demonstrates embeddings and semantic search for RAG pipelines
// Run with: zynml run examples/vector_search.ml

fn main() {
    println("=== ZynML Vector Search ===")

    // Create some sample vectors (in practice, these would be embeddings)
    let doc1 = tensor([0.1, 0.2, 0.3, 0.4])
    let doc2 = tensor([0.5, 0.1, 0.2, 0.1])
    let doc3 = tensor([0.2, 0.3, 0.4, 0.5])

    // Normalize vectors for cosine similarity
    doc1 |> vec_normalize()
    doc2 |> vec_normalize()
    doc3 |> vec_normalize()

    // Create a flat index for exact search
    let index = flat_create(4)

    // Add documents to index
    flat_add(index, 1, doc1)
    flat_add(index, 2, doc2)
    flat_add(index, 3, doc3)

    println("Added 3 documents to index")

    // Query vector
    let query = tensor([0.15, 0.25, 0.35, 0.45])
    query |> vec_normalize()

    // Compute similarities manually
    let sim1 = vec_cosine(query, doc1)
    let sim2 = vec_cosine(query, doc2)
    let sim3 = vec_cosine(query, doc3)

    println("Cosine similarities:")
    println("  Doc1: ")
    println("  Doc2: ")
    println("  Doc3: ")

    // Search index for top-2 results
    // let results = index |> flat_search(query, 2)

    println("Search complete!")
}
