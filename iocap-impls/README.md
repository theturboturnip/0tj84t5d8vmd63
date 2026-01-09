# crypto-cap-experiments

Experiments with cryptographic functions to find a good signature function/format for cryptography-based decentralized capabilities.

**DO NOT USE IN PRODUCTION - THE SECURITY OF THESE FUNCTIONS HAS NOT BEEN TESTED YET.**

## Project Structure
- `/src/` holds Rust implementations of AES and multiple capability formats, including "2024-02" which is the current working prototype.
- `/python/` holds Python notebooks which explore different formats for compressing 64-bit base and length, among other things
- `/viz_html/` holds the results of some visualizations of some of these formats
- `/bluespec/` holds Bluespec implementations of a checker for "2024-02" (WORK IN PROGRESS)

## Building/running
1. [Install Rust v1.67+](https://www.rust-lang.org/tools/install)
2. Install OpenSSL and pkg-config on Linux - required for cryptographic functions and the Rust openssl package
   ```bash
   # Ubuntu
   sudo apt-get install libssl-dev pkg-config
   ```
3. Run the tests
   ```bash
   cargo test
   ```