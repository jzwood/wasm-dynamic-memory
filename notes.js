
function malloc(size) {
  const HEADER = 4

  let ptr_last = get_memory_size()

  let ptr_prev = 0
  let size_prev = 0
  let free_prev = 0

  let ptr_next = 0
  let size_next = 0
  let free_next = 0

  while (true) {
    ptr_next = ptr_prev + size_prev

    if (ptr_next > ptr_last) {
      grow_memory()
      ptr_last = get_memory_size()
    }

    let [ size_next, free_next ] = load(ptr_next)

    if (free_prev === 0 && free_next === 0) {
      // neither block is free -- do nothing
      continue
    }

    if (free_prev === 1 && free_next === 0) {
      // current block not free
      // rewrite ptr_prev block to cover consecutive free blocks
      store(ptr_prev, size_prev, 1)
      continue
    }

    if (free_prev === 0 && free_next === 1) {
      size_prev = size_next
      free_prev = free_next
      ptr_prev = ptr_next
    } else if (free_prev === 1 && free_next === 1) {
      // both free -- extend size_prev
      size_prev = size_prev + size_next
      //free_prev = free_next ;; redundant
      ptr_prev = ptr_next
    }

    if (size_prev === size) {
      store(ptr_prev, size, 0)
      return ptr_prev
    } else if (size_prev > size) {
      store(ptr_prev, size, 0)
      store(ptr_prev + size, size_prev - size, 1)
      return ptr_prev
    }
  }
}
