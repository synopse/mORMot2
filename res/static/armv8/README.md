# mORMot ARMV8 Static

This folder contains optimized C code which compiles into SIMD ARMV8 assembly, using hardware accelerated opcodes for crc32, crc32c, aes encryption/decryption, gcm multiplication and sha-256 process.

On supported hardware, it reaches high performance.

# Compilation Notice

We used external C code and static linking because the FPC internal asm is very limited, and don't include even the whole AARCH64 standard opcodes. So it is impossible to include those asm into the mORMot source code tree, as we do for Intel/AMD.

This source code is included as reference. Please don't try to compile the static files by yourself, but download them from the latest https://github.com/synopse/mORMot2/releases or https://synopse.info/files/mormot2static.7z
