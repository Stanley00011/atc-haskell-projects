# Project 3: Multi-Agent Simulation with Autonomous Behavior

## How to Run the Application

TODO

## High-Level Description

This project requires building a simple multi-agent environment in which autonomous agents pursue individual or shared goals. Agents should be able to sense their surroundings, make decisions based on state information, and interact with each other—either cooperatively or competitively. The environment can be 2D grid-based (e.g., warehouse, maze, predator-prey) and must be fully simulated. The goal is to demonstrate understanding of agent-based modeling, reinforcement learning or rule-based behavior, and emergent dynamics.

## Software Requirements

### Basic Functionality:

- Implement a 2D grid or discrete simulation environment.
- Include multiple agents with distinct goals or roles.
- Agents make decisions autonomously at each timestep.
- Log actions, states, and performance metrics at each step.

### Agent Design:

- Use rule-based logic, behavior trees, or reinforcement learning.
- Agents must act based on partial or full observations.
- Support agent-agent interaction (e.g., collisions, communication, blocking).

### Simulation:

- Run for a fixed number of steps or until task completion.
- Track metrics like goal completion rate, efficiency, or survival.

### Visualization:

- Render the environment with matplotlib, pygame, or ASCII.
- Optional: Animate agent trajectories over time.

### Optional Advanced Features:

- Use a reinforcement learning algorithm (e.g., PPO, Q-learning).
- Implement communication between agents.
- Include curriculum learning or dynamic difficulty adjustment.

## Acceptance Criteria:

### Functionality:

- Simulation runs with multiple agents making decisions each step.
- Agent actions are logged or displayed.

### Agent Behavior:

- Decisions are made from state, not hardcoded sequences.
- Agents pursue clear, defined objectives.

### Code Quality:

- Environment and agents are modular and reusable.
- Logging and config options are included.

### Visualization:

- Simulation is visually inspectable at runtime or post-run.
- Visuals help explain system dynamics.

## Rubric:

### Basic Functionality (40 points):

- Environment setup and simulation loop (10 points)
- Multiple autonomous agents implemented (10 points)
- Distinct goals and interaction logic (10 points)
- Action/state logging or result output (10 points)

### Agent Behavior (20 points):

- Autonomous decision logic (10 points)
- Dynamic or conditional behavior (10 points)

### Visualization (10 points):

- Clear environment rendering (5 points)
- Agent movement or state visibility (5 points)

### Code Quality and Structure (20 points):

- Modular design and code separation (10 points)
- Readability, comments, and config support (10 points)

### Advanced Features (Optional - 20 points):

- RL algorithms, communication, or advanced simulation dynamics (10–20 points)

**Total: 120 points (100 if advanced features are not implemented)**
