# Project 1: Retrieval-Augmented Generation (RAG) Web App

## How to Run the Application

TODO


## High-Level Description

This project implements a Retrieval-Augmented Generation (RAG) web application that allows users to ask natural language questions over a custom corpus of documents. The system retrieves the most relevant document chunks using semantic search and then feeds them into a large language model to generate contextual answers. The goal is to build an end-to-end, scalable system that demonstrates your ability to integrate modern NLP models with backend infrastructure and front-end usability.


## Software Requirements

### Basic Functionality:
- The system must allow users to ask natural language questions over a provided corpus (e.g., markdown files, PDFs, or scraped data).
- The system must retrieve relevant document chunks and pass them into a language model to generate an answer.
- Returned answers must include source citation metadata (e.g., titles or links).

### Backend:
- Uses an embedding model to vectorize documents (e.g., OpenAI, HuggingFace, or SentenceTransformers).
- Stores vectors in a vector store (e.g., FAISS, Weaviate, Qdrant).
- Uses a language model (e.g., OpenAI GPT, Mistral, or LLaMA) to generate final answers.
- Backend must expose an API endpoint that takes in a query and returns an answer.

### Frontend:
- A simple React/Vue/HTML interface that allows users to enter a query and view the response.
- Responses must be displayed in a readable format with citation metadata.

### Deployment:
- The application must run in a containerized environment (e.g., Docker Compose or Kubernetes).
- Environment variables must be used for secrets (e.g., API keys).
- Logging and error handling should be included.

### Optional Advanced Features:
- Real-time streaming responses (via Server-Sent Events or WebSockets).
- Multi-turn conversation support with memory.
- Support for multiple corpora and corpus selection.
- Query re-ranking or hybrid search (BM25 + embeddings).

## Acceptance Criteria:

### Functionality:
- User can input a natural language question and receive an answer with sources.
- Corpus must be indexed and searchable via embedding vectors.
- Language model outputs are relevant and accurate.

### Usability:
- The UI should be easy to use and responsive.
- Answers should include links or labels for sources.

### Code Quality:
- The backend must be modular and well-documented.
- Use of configuration files (e.g., `.env`, YAML) for environment setup.
- Logging should capture relevant events and errors.

### Deployment:
- App can be deployed locally using Docker.
- Documentation should describe how to deploy and test the system.

## Rubric:

### Basic Functionality (40 points):
- Query input and answer display (10 points)
- Embedding-based document retrieval (10 points)
- LLM integration for generation (10 points)
- Source citation in output (10 points)

### Backend Engineering (20 points):
- Vector store integration (10 points)
- RESTful API or endpoint (10 points)

### Frontend/User Interface (10 points):
- Clean query input/output UI (5 points)
- Citation display clarity (5 points)

### Deployment and Infrastructure (20 points):
- Dockerization and container setup (10 points)
- Use of environment variables and config (10 points)

### Code Quality and Documentation (10 points):
- Modular, readable code and inline comments (5 points)
- Clear README with setup and usage instructions (5 points)

### Advanced Features (Optional - 20 points):
- Streaming responses (10 points)
- Multi-turn memory or hybrid search (10 points)

**Total: 120 points (100 if advanced features are not implemented)**

