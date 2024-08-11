### HEAP DESIGN 1)

| name                 | description |
|----------------------|-----------------------------------------------------|
| free<br>size         | is data available to be allocated<br>size of data |
| data                 | user data |
| prev free<br> offset | is prev data available to be allocated<br>offset from free ptr (VLE) |

#### Variable Length Encoding (VLE)

| bytes | free  | length-prefix | size/offset       |
|-------|-------|---------------|-------------------|
| 1     | 1 bit | 0             | 6 bits            |
| 2     |       | 10            | 5 bits + 1 byte)  |
| 3     |       | 110           | 4 bits + 2 bytes) |
| 4     |       | 1110          | 3 bits + 3 bytes) |
| 5     |       | 11110         | 2 bits + 4 bytes) |

_notes:_ modifying ULEB128 would be an interesting VLE alternative.

#### Examples
```
malloc 2
10000002 00000000 00000000 1000000003

malloc 1, malloc 1
10000001 00000000 10000002 10000001 00000000 1000002

malloc 65
11000000 01000001 [00000000 * 65] 11000000 01000011
```

malloc(size):
1. finds the first free block that can fit data
2. divide block into allocated block and free block respectively
3. for each block
   1. set free bit and size
   2. set prev free and offset

### HEAP DESIGN 2)

| size    | free  | data | size ptr | free  |
|---------|-------|------|----------|-------|
| 31 bits | 1 bit |      | 31 bits  | 1 bit |

Doubly linked list

### HEAP DESIGN 2)

| size     | free  | data |
|----------|-------|------|
| 31 bits  | 1 bit |      |

- malloc(size) ->
   - searches for contiguous free blocks >= size
   - merges free blocks during search
- free(ptr) -> sets free to 0
