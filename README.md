# Calyxium: **An Interpreted Programming Language**

<!-- TOC -->

<p align="left"> 
    <a href="#installation">Installation</a> | 
    <a href="#getting-started">Getting Started</a> | 
    <a href="#usage">Usage</a> | 
    <a href="#contributing">Contributing</a> | 
    <a href="#license">License</a>
</p>

<!-- TOC -->

## Installation
Follow these steps to install and build Calyxium:

1. Install OCaml

    Download and install OCaml from the [official site](https://ocaml.org/install)
    - [Linux/macOS/BSD](https://ocaml.org/install#linux_mac_bsd)
    - [Windows](https://ocaml.org/install#windows)

2. Clone the repository
    ```bash
    git clone https://github.com/Calyxium-Lang/Calyxium.git
    cd Calyxium
    ```

4. Install dependencies
    ```bash
    opam install menhir zarith ANSITerminal
    ```

4. Build the projrct
    ```bash
    dune build
    ```

## Getting Started
Create a new Calyxium script with the `.cx` extension. Here's a basic example:
```
# This is a comment in Calyxium
println("Hello, world")
```
To run your script:
`./calyxium main.cx`

## Usage
Calyxium is designed to be expressive and easy to use. Here are some common features:
- **Printing**
    ```
    println("Hello, world")
    ```
- **Variables**
    ```
    let x: int = 10
    ```
- **Conditionals**
    ```
    if (x > 5) {
        println("x is greater than 5")
    }
    ```

For more detailed documentation, visit the [official documentation](https://calyxium.cc/docs)

## Contributing
We welcome contributions! Whether it's a bug report, feature suggestion, or code contribution:

- File issues via the [issue tracker](http://github.com/Calyxium-Lang/Calyxium/issues)
- For the repo and open a pull request

Please follow conventional commit and PR practices where possible.

## License

Calyxium is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
