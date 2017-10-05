# Function Stats/Divvying Up Work

### Ami ( > 56 lines) (actual: 47 lines)
- [x] `writeToFile` ( 12-15 lines) (actual: 46 lines)
- [x] `activationFunction()` (1 line) (actual: 1 line)
- [ ] `readInit(file)` (20-30 lines)
- [ ] `getAt()` ( > 10 lines)

### Elijah ( > 57 lines) (actual: 23 lines)
- [x] `costPrime` (1 line) (actual: 1 line)
- [x] `distribution` (> 5 lines) (actual: 14 lines)
- [x] `hadamardProduct` ( > 7 lines) (actual: 1 line)
- [x] `update` (3 lines) (actual: 4 lines)
- [x] `randomizeMatrix` ( < 5 lines) (actual: 3 lines)
- [ ] `forwardPropagation` ( > 5 lines)
- [ ] `classify` ( > 30 lines)
- [ ] `activationPrime` (1 line)

### Yonatan ( > 45 lines ) (actual: 30 lines)
- [x] `readInit() //cin` (7-15) (actual: 25 lines)
- [x] `shuffle(vector<T>)` (~5) (actual: 5 lines)
- [ ] `Network` default constructor ( < 25 lines)

## Approximate Length of Functions
| Function Name | Appx. Lines, based on Pseudocode.cpp |
| ------------- | ----------------------- |
| `Network(file, file)` | < 40 |
| `train` | < 30 |
| `Network(file)` | < 25 |
| `backProp` | < 20 |
| `SGD` | < 20 |
| `Network(cin)` | < 20 |
| `classify` | < 20 |
| ~~`writeToFile`~~ | < 15 |
| `getAt` | guess: < 15 |
| `forwardProp` | < 10 |
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
| ~~`hadamardProduct`~~ |  `forwardProp` | `readInit(file)` |
| ~~`shuffleDataIndices`~~ | `classify` | `backProp` |
| ~~`writeToFile`~~ | `activationPrime` | `SGD` |
| ~~`update`~~ | `Network(cin)` | `train` |
| ~~`distribution`~~ | `getAt` |
| ~~`readInit(cin)`~~ |
