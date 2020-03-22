cargo xrun --target x86_64-jitos.json -- -device isa-debug-exit,iobase=0xf4,iosize=0x04 -serial stdio | Out-File -FilePath ./out.txt
