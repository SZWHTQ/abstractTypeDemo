# Fortran Abstract Type Demo
## Build
```bash
cmake -B build
cmake --build build
```
## Test
```bash
./build/main < input | grep "line"
```
### Output
```
line: x = 11.000000000000000 y = 12.000000000000000 l = 13.000000000000000 
line: x = 21.000000000000000 y = 22.000000000000000 l = 23.000000000000000 
line: x = 31.000000000000000 y = 32.000000000000000 l = 33.000000000000000
```