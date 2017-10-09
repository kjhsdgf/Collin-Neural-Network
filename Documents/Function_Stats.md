# Function Stats/Divvying Up Work

### Ami ( > 80 lines) (actual ~ 144 lines)
- [x] `writeToFile` ( 12-15 lines) (actual: 46 lines)
- [x] `activationFunction()` (1 line) (actual: 1 line)
- [x] `readInit(file)` (20-30 lines) (actual ~ 50 lines)
- [x] `getAt()` ( > 10 lines) (actual: 13 lines)
- [x] `Strtok(string)` (6-8 lines) (actual: 16 lines)
- [x] `Network(file)`(8-10 lines) (actual: 16 lines)
- [x] `~Network()` (5-6 lines) (actual: 2 lines)


### Elijah ( > 82 lines) (actual: 132 lines)
- [x] `costPrime` (1 line) (actual: 1 line)
- [x] `distribution` (> 5 lines) (actual: 14 lines)
- [x] `hadamardProduct` ( > 7 lines) (actual: 1 line)
- [x] `updateWeightsAndBiases` (3 lines) (actual: 4 lines)
- [x] `randomizeMatrix` ( < 5 lines) (actual: 3 lines)
- [x] `forwardPropagation` ( > 5 lines) (actual: 4 lines)
- [x] `classify(file)` ( > 30 lines) (actual: 60 lines)
- [x] `classify()` (mostly copy/paste)
- [x] `activationPrime` (1 line) (actual: 1 line)
- [x] `Network()` default constructor ( < 25 lines) (actual: 44 lines)

### Yonatan ( > 45 lines ) (actual: 30 lines)
- [x] `readInit() //cin` (7-15) (actual: 25 lines)
- [x] `shuffle(vector<T>)` (~5) (actual: 5 lines)
- [x] `SGD()` ( >10 lines) (actual: 10)
- [x] `backProp()` ( <20 lines) (actual: 15)
- [ ] `train()` ( <30 lines)

## Approximate Length of Functions
| Function Name | Appx. Lines, based on Pseudocode.cpp |
| ------------- | ----------------------- |
| `Network(file, file)` | < 40 |
| `train` | < 30 |
| `Network(file)` | < 25 |
| `backProp` | < 20 |
| `SGD` | < 20 |
| ~~`Network(cin)`~~ | < 20 |
| ~~`classify(file)`~~ | < 20 |
| ~~`classify()`~~ | < 20 |
| ~~`writeToFile`~~ | < 15 |
| ~~`getAt`~~ | guess: < 15 |
| ~~`forwardProp`~~ | < 10 |
| ~~`distribution`~~ | guess: < 10 |
| ~~`hadamardProduct`~~ | 7 |
| ~~`readInit(cin)`~~ | 6 |
| `readInit(file)` | > 6 |
| `~Network()` | < 5 |
| ~~`randomizeMatrix`~~ | < 5 |
| ~~`shuffleDataIndices`~~ | < 5 |
| ~~`updateWeightsAndBiases`~~ | 3 |
| ~~`activationPrime`~~ | 2 |
| ~~`costPrime`~~ | 1 |
| ~~`activation_Function`~~ | 1 |

## Function Dependencies
### functions that depend on other functions being written prior
| No prerequisites | At least 1 in previous category| At least 2 in previous categories|
| ---- | ---------- | ---------- |
| ~~`costPrime`~~ | `~Network()` | `Network(file)` |
| ~~`activationFunction`~~ | ~~`randomizeMatrix`~~ | `Network(file, file)` |
| ~~`hadamardProduct`~~ |  `forwardProp` | `readInit(file)` |
| ~~`shuffleDataIndices`~~ | ~~`classify`s~~ | `backProp` |
| ~~`writeToFile`~~ | ~~`activationPrime`~~ | `SGD` |
| ~~`updateWeightsAndBiases`~~ | ~~`Network(cin)`~~ | `train` |
| ~~`distribution`~~ | ~~`getAt`~~ |
| ~~`readInit(cin)`~~ |
