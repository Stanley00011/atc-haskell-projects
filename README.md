# Project 2: Train a Deep Learning Model from Scratch

## How to Run the Application

TODO

## High-Level Description

This project involves designing and training a deep learning model from scratch on a real-world dataset. The focus is on demonstrating your understanding of data preprocessing, model architecture, training loops, evaluation, and optimization. You are expected to avoid pre-trained models and instead construct your own model using a framework like PyTorch or TensorFlow. The goal is to produce a well-documented, reproducible training pipeline and analyze the results rigorously.

## Software Requirements

### Basic Functionality:
- Load a dataset from a standard source (e.g., CIFAR-10, IMDB, MNIST, custom CSV).
- Preprocess the data for training and validation (normalization, tokenization, etc.).
- Define a model architecture (e.g., CNN, MLP, RNN, Transformer).
- Train the model with a validation split and log training metrics.
- Save the trained model and evaluation results.

### Evaluation:
- Evaluate the model on a holdout test set using appropriate metrics (accuracy, F1, MSE, etc.).
- Provide loss curves and relevant visualizations.

### Reproducibility:
- Code should produce the same results with a fixed seed.
- Dependencies should be documented and versioned.

### Optional Advanced Features:
- Hyperparameter tuning (grid/random search).
- Data augmentation pipeline.
- Custom loss functions or layers.
- Experiment tracking (e.g., with Weights & Biases).

## Acceptance Criteria:

### Functionality:
- Model trains successfully on the chosen dataset.
- Evaluation metrics are computed and logged.

### Usability:
- Scripts are clearly separated (e.g., train vs evaluate).
- Parameters are configurable (e.g., via argparse or config file).

### Code Quality:
- Model code is modular and separated from training logic.
- Good naming conventions and clear function boundaries.

### Reproducibility:
- Runs with fixed seed produce similar results.
- All dependencies are pinned (e.g., `requirements.txt`).

## Rubric:

### Basic Functionality (40 points):
- Data loading and preprocessing (10 points)
- Custom model architecture (10 points)
- Training loop with logging (10 points)
- Model saving and evaluation (10 points)

### Evaluation and Analysis (20 points):
- Correct use of evaluation metrics (10 points)
- Visualization of training/validation metrics (10 points)

### Reproducibility (10 points):
- Fixed seed and deterministic behavior (5 points)
- Complete dependency specification (5 points)

### Code Quality and Organization (20 points):
- Modular design and clear file structure (10 points)
- Use of best practices in code (10 points)

### Advanced Features (Optional - 20 points):
- Hyperparameter search, augmentation, or experiment tracking (10–20 points)

**Total: 120 points (100 if advanced features are not implemented)**

