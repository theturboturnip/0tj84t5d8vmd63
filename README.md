This is a repository of all artifacts associated with our IOCap paper submission.
It contains copies of our source code for our CheriBSD fork, CheriFreeRTOS fork, software-emulated VirtIO peripherals, QEMU fork, Stratix-10 FPGA SoC and IOCap hardware, and software IOCap implementations.

## Naming & Aliases

Some source code files refer to concepts from the paper with different names:
- IOCaps were previously known as Crypto-Caps or CCaps.
- The VirtIO IOCap format presented in the paper is referred to as the "Cap2024_11" format.
- The Lease Manager was known as the Key Manager or `kmngr`.
- The Enforcer was known as the Exposer.

## Relevant Files

This repository contains the following sub-repositories:

- `iocap-impls/`, software libraries written in Rust, C, and Python for parsing and generating IOCaps
    - `rust_caps/` is a Rust-native IOCap parser/generator with tests in `rust_caps/src/capability/v2024_11/test.rs`.
    - `rust_caps_c/` is a Rust library which exposes a C-compatible interface to `rust_caps/`, used by `tinyemu-virtio` and the hardware testbenches in `de10pro-cheri-bgas/bluespec/IOCapAxi/testbenches/`, as a static library and header file.
    - `rust_caps_testgen/` is a Rust crate which generates test vectors for other IOCap implementations.
    - `tests_cap2024_*/` stores these test vectors.
    - `libccap/` is a pure-C library which mirrors a subset of the `rust_caps_c/` functionality, used in our CheriBSD, CheriFreeRTOS, and QEMU forks.
    - `python/cap2024_11_decoder_test.py` is a Python IOCap decoder.
- `de10pro-cheri-bgas/`, our fork of the CHERI BGAS SoC, including IOCap hardware implementations.
    - `bluespec/IOCapAxi/` includes IOCap hardware modules.
        - `aes/` is an AES library.
        - `cap2024/` includes IOCap decoders and signature checking modules.
        - `IOCapAxi_Exposers_V6.bsv` defines the basic Enforcer unit described in the paper.
        - `IOCapAxi_Checker3s.bsv` defines the basic Checker unit and the Checker Pools described and evaluted in the paper.
        - `IOCapAxi_KeyManager2_*.bsv` collectively define the Lease Manager described and evaluted in the paper.
        - `IOCapAxi_Windows.bsv` defines an AXI 'window' which converts plain AXI DMA requests into IOCapAXI DMA requests by attaching metadata, used by software-emulated peripherals.
        - `testbenches/` contains a comprehensive set of testbenches for various Enforcer configurations, using `iocap-impls/rust_caps_c` for random IOCap generation.
            - `exposer_tests_uvm.h` contains the main test definitions.
    - `bluespec/Toooba/` is the CHERI-compatible out-of-order superscalar soft-CPU used in FPGA tests.
    - `bluespec/CHERI_BGAS_System.bsv` is the main SoC definition, where line 368 integrates the IOCap modules.
        - This uses older, lower-performance Enforcer and Lease Manager modules that pass the same testbenches:
        - `IOCapAxi_Exposers_V4.bsv` and `IOCapAxi_KeyManagers.bsv`.
- `cheribsd/`, our fork of CheriBSD with IOCap support.
    - `sys/dev/iocap/` contains the driver for the Lease Manager.
        - `iocap_keymngr.{c,h}` define the driver itself, including lease allocation and random key generation.
        - `libccap/` is a C library for generating IOCaps, vendored from `iocap-impls`.
    - `sys/dev/virtio/` contains the VirtIO driver infrastructure.
        - `virtio_config.h` and `virtio_bus_if.m` define IOCap helpers and flags.
        - `mmio/virtio_mmio.{c,h}` includes basic functions for MMIO-based VirtIO-IOCap drivers.
        - `virtqueue_iocap.{c,h}` and `virtio_ring_iocap.h` define a VirtIO IOCap ring buffer queue.
        - `block/virtio_blk_iocap.c` defines a VirtIO IOCap block device driver.
- `FreeRTOS/`, our fork of CheriFreeRTOS with IOCap support.
    - `FreeRTOS-Labs/FreeRTOS-Labs/Source/FreeRTOS-libvirtio/` defines the VirtIO/IOCap drivers.
        - `iocap/libccap*` is a C library for generating IOCaps, vendored from `iocap-impls`.
        - `virtio_mmio.h` and `virtio.h` include IOCap utility functions.
        - `virtio.c` includes lease manager initialization using static predefined keys and updates generic VirtIO descriptor interaction functions to use IOCaps instead, automatically allowing virtio-net and virtio-blk drivers to support IOCaps.
- `tinyemu-virtio/`, a set of software-emulated VirtIO peripherals with VirtIO-IOCap support based on <https://bellard.org/tinyemu/>.
    - `src/iocap/` contains Arm and x86 copies of the `rust_caps_c/` libraries from `iocap-impls`, for use on the Stratix 10 hard Arm CPU and x86 simulation machines respectively.
    - `src/virtio.{c,h}` include updated generic functions for handling VirtIO descriptors that transparently change to handle IOCaps when necessary, enabling IOCap support in all peripherals.
        - It also ensures source IOCaps are carried with all DMA attempts so that they can be used for authentication.
    - `src/fpga.{cpp,h}` are updated to write IOCaps into the DMA "window" peripheral defined in `de10pro-cheri-bgas` to attach them to outgoing AXI transactions.
- `qemu/`, our fork of CHERI-compatible QEMU with IOCap support
    - `hw/iocap/libccap*` include the headers and source files for generating IOCaps, vendored from `iocap-impls`.
        - Relevant headers are in `include/hw/iocap/libccap-0.5.0/`
    - `hw/iocap/iocap_kmngr.c` define the emulated Lease Manager peripheral.
        - Relevant header is `include/hw/iocap/iocap_kmngr.h`
    - `hw/riscv/virt.c` inserts the emulated Lease Manager into the RISC-V device tree.
        - Relevant header is `include/hw/riscv/virt.h`
    - `hw/virtio/virtio{-mmio,}.c` include updated functions for interacting with VirtIO IOCap descriptor queues and checking accesses
        - Relevant headers are `include/hw/virtio/virtio{-mmio,-iocap,}.h` and `include/standard-headers/linux/virtio_{config,mmio}.h

<!--
- `pci/virtio_pci*.c` stub out IOCap support for PCI-based VirtIO drivers.
- `sys/riscv/conf/GENERIC`
- `sys/riscv/riscv/busdma_bounce.c`
- `sys/riscv/riscv/pmap.c`
- `sys/x86/x86/busdma_bounce.c` non-IOCap bugfixes

FreeRTOS
- `virtio-net.c` includes debug info

tinyemu
- `virtiodevices.*` pass an iocap-enable bool through the system

-->

All source code copyright notices reflect the original authors of the pre-fork code, not the authors of this paper, except for a few instances where the relevant names have been anonymized.

## Omissions

This repository is intended to be anonymized via <https://anonymous.4open.science>, which has a 2GB-per-user limit.
To avoid this limit we have removed the following irrelevant, redundant, or binary files:

- `cheribsd/contrib` took up ~650MB of source files unrelated to IOCaps
- `cheribsd/sys/contrib` took up ~370MB of source files unrelated to IOCaps
- `qemu/roms` took up ~660MB and contained binary boot ROMs unrelated to IOCaps
- `iocap-impls/hardware` took up ~200MB and contained legacy build systems and old copies of the hardware files now stored in `de10pro-cheri-bgas/bluespec/IOCapAxi/`
- Copies of `librust_caps_c.a` in `tinyemu-virtio` and `de10pro-cheri-bgas`, static library files built from the Rust code in `iocap-impls/rust_caps_c`, took up 20MB each and have been replaced with txt files.

We have also removed `*.git*` files throughout.