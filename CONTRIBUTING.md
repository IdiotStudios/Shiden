# Contributing to Shiden

Going to more Soon

Install nasm: (feel free to do a pr to update this)
```bash
sudo pacman -S nasm # Arch Linux
sudo apt install nasm # Debian/Ubuntu
```

Install asmfmt:
```bash
go install github.com/klauspost/asmfmt/cmd/asmfmt@latest

# add go to path if not already
echo 'export PATH="$HOME/go/bin:$PATH"' >> ~/.bashrc
```

Run it with
```bash
find libraries -name '*.asm' -exec asmfmt -w {} +
```

## Licensing

By contributing you agree that your changes will be licensed under the repo's MIT license, see `LICENSE`.

## Need help?

Open an issue describing the problem or feature you'd like to work on, or start a draft PR and ask for review. We're happy to review and help iterate.

---