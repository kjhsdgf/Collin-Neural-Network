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
| ~~`writeToFile`~~ | < 15 |
| `forwardPropagation` | < 10 |
| ~~`distribution`~~ | guess: < 10 |
| ~~`hadamardProduct`~~ | 7 |
| ~~`readInit(cin)`~~ | 6 |
| `readInit(file)` | > 6 |
| `~Network()` | < 5 |
| ~~`randomizeMatrix`~~ | < 5 |
| ~~`shuffleDataIndices`~~ | < 5 |
| ~~`update`~~ | 3 |
| `activationPrime` | 2 |
| ~~`costPrime`~~ | 1 |
| ~~`activation_Function`~~ | 1 |

## Function Dependencies
### functions that depend on other functions being written prior
| No prerequisites | At least 1 in previous category| At least 2 in previous categories|
| ---- | ---------- | ---------- |
| ~~`costPrime`~~ | `~Network()` | `Network(file)` |
| ~~`activationFunction`~~ | ~~`randomizeMatrix`~~ | `Network(file, file)` |
| ~~`hadamardProduct`~~ |  `forwardPropagation` | `readInit(file)` |
| ~~`shuffleDataIndices`~~ | `classify` | `backPropagation` |
| ~~`writeToFile`~~ | `activationPrime` | `SGD` |
| ~~`update`~~ | `Network(cin)` | `train` |
| ~~`distribution`~~ |
| ~~`readInit(cin)`~~ |
