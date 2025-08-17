# Calyxium: **An Interpreted Multi-Paradigm Programming Language**

<!-- TOC -->

<p align="left"> 
    <a href="#why">Why</a> |
    <a href="#installation">Installation</a> | 
    <a href="#getting-started">Getting Started</a> | 
    <a href="#usage">Usage</a> | 
    <a href="#contributing">Contributing</a> | 
    <a href="#license">License</a>
</p>

<!-- TOC -->

![Build Status](https://github.com/calyxium-lang/calyxium/actions/workflows/ci.yml/badge.svg)

## Why
Going to be real, Calyxium exists because I needed a project impressive enough to skip some classes in my math degree.

## Installation
Follow these steps to install and build Calyxium:

1. Install OCaml

    Download and install OCaml from the [official site](https://ocaml.org/install)
    - [Linux/macOS/BSD](https://ocaml.org/install#linux_mac_bsd)
    - [Windows](https://ocaml.org/install#windows)

2. Clone the repository
    ```bash
    git clone https://github.com/calyxium-lang/calyxium.git
    cd calyxium
    ```
    
3. Install the dependency
    ```bash
    dune install menhir
    ```

4. Build the projrct
    ```bash
    dune build --profile release
    ```

5. Install the project
    ```bash
    dune install
    ```

## Getting Started
Create a new Calyxium script with the `.cx` extension. Here's a basic example:
```
print("Hello, world\n")
```
To run your script:
`calyxium main.cx`

For more detailed documentation, visit the [official documentation](https://calyxium-lang.github.io/docs)

## Contributing
We welcome contributions! Whether it's a bug report, feature suggestion, or code contribution:

- File issues via the [issue tracker](http://github.com/calyxium-lang/calyxium/issues)
- For the repo and open a pull request

Please follow [conventional commit and PR practices where possible](CONTRIBUTING.md).

## License

Calyxium is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
