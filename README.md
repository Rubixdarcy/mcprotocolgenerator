# mcprotocolgenerator
![protocol](https://img.shields.io/static/v1?label=Protocol&message=1.17.1&color=brightgreen)

Consume data from [PrimarineJS minecraft data](https://github.com/PrismarineJS/minecraft-data) and generate source code that serializes and deserializes the minecraft protocol.

## Build
Build using Stack.
```
stack build
```

## Usage
Currenty only Python is supported using the [Construct](https://construct.readthedocs.io/en/latest/) library.
```
stack run minecraft-data 1.17.1
```
The output will be placed in the directory `v1_17_1`.

## Supported languages

| Language         | Status |
| ---------------- | ------ |
| Python Construct | ✅      |
| Rust             | 🛠️      |
