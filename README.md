
| free (1 bit) | size (n bytes - 1 bit)     | data (size bytes) | prev free (1 bit) | offset (size + size byte length) |
|--------------|----------------------------|-------------------|-------------------|----------------------------------|
| x            | 0xxxxxx (6 bits)           |                   | x                 | 0xxxxxx (6 bits)                 |
| x            | 10xxxxx (5 bits + 1 byte)  |                   | x                 | 10xxxxx (5 bits + 1 byte)        |
| x            | 110xxxx (4 bits + 2 bytes) |                   | x                 | 110xxxx (4 bits + 2 bytes)       |
| x            | 1110xxx (3 bits + 3 bytes) |                   | x                 | 1110xxx (3 bits + 3 bytes)       |
| x            | 11110xx (2 bits + 4 bytes) |                   | x                 | 11110xx (2 bits + 4 bytes)       |



   malloc:
   use next_ptr to jump blocks until you reach an free block
   if node is smaller than requested block:
      update next size
      set next free = 1

      compare pointer current pointer with next_ptr to see if empty block is big enough
   else jump to next node and repeat from top


