# Function Stats
### My hope is that this will help produce a roughly equivalent distribution of work that is easier to distribute!

## Approximate Length of Functions
| Function Name | Appx. Lines, based on Pseudocode.cpp |
| ------------- | ----------------------- |
| `Network(file, file)` | < 40 |
| `train` | < 30 |
| `Network(file)` | < 25 |
| `backPropagation` | < 20 |
| `SGD` | < 20 |
| `Network(cin)` | < 20 |
| `classify` | < 20 |
| _`writeToFile`_ | < 15 |
| `forwardPropagation` | < 10 |
| _`distribution`_ | guess: < 10 |
| _`hadamardProduct`_ | 7 |
| _`readInit(cin)`_ | 6 |
| `readInit(file)` | > 6 |
| `~Network()` | < 5 |
| _`randomizeMatrix`_ | < 5 |
| _`shuffleDataIndices`_ | < 5 |
| _`update`_ | 3 |
| `activationPrime` | 2 |
| _`costPrime`_ | 1 |
| _`activation_Function`_ | 1 |

## Function Dependencies
### functions that depend on other functions being written prior
| No prerequisites | At least 1 in previous category| At least 2 in previous categories|
| ---- | ---------- | ---------- |
| _`costPrime`_ | `~Network()` | `Network(file)` |
| _`activationFunction`_ | _`randomizeMatrix`_ | `Network(file, file)` |
| _`hadamardProduct`_ |  `forwardPropagation` | `readInit(file)` |
| _`shuffleDataIndices`_ | `classify` | `backPropagation` |
| _`writeToFile`_ | `activationPrime` | `SGD` |
| _`update`_ | `Network(cin)` | `train` |
| _`distribution`_ |
| _`readInit(cin)`_ |
