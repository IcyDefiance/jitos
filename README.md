# JitOS

## Building

You need a nightly Rust compiler. First you need to install the `cargo-xbuild` and `bootimage` tools:

```
cargo install cargo-xbuild bootimage
```

Then you can build the project by running:

```
cargo xbuild --target x86_64-jitos.json
```

To create a bootable disk image, run:

```
cargo bootimage
```

This creates a bootable disk image in the `target/x86_64-jitos/debug` directory.

Please file an issue if you have any problems.

## Running

You can run the disk image in [QEMU] through:

[qemu]: https://www.qemu.org/

```
cargo xrun --target x86_64-jitos.json
```

Of course [QEMU] needs to be installed for this.

You can also write the image to an USB stick for booting it on a real machine. On Linux, the command for this is:

```
dd if=target/x86_64-jitos/debug/bootimage-jitos.bin of=/dev/sdX && sync
```

Where `sdX` is the device name of your USB stick. **Be careful** to choose the correct device name, because everything on that device is overwritten.

## Testing

To run the unit and integration tests, execute `cargo xtest --target x86_64-jitos.json`.
