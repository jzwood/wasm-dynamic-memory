(module
  (import "console" "log" (func $log (param i32)))
  (memory (export "memory") 1)
  (global $bytes_per_page (mut i32) (i32.const 65536))
  (global $span (mut i32) (i32.const 0))

  ;; HEAP
  ;; | size     | free  | data |
  ;; |----------|-------|------|
  ;; | 31 bits  | 1 bit |      |

  (func $inspect (param $x i32) (result i32)
    local.get $x
    call $log
    local.get $x
  )

  (func (export "init")
    i32.const 0
    global.get $bytes_per_page
    i32.const 4
    i32.sub
    i32.const 1
    i32.shl
    i32.const 1
    i32.and
    i32.store
  )

  (func $free (export "free") (param $ptr i32)
    local.get $ptr
    local.get $ptr
    i32.load
    i32.const 0
    i32.and
    i32.store
  )

  (func $encode (param $ptr i32) (param $free i32) (result i32)
    local.get $ptr
    i32.const 1
    i32.shl
    local.get $free
    i32.or
  )

  (func $decode (param $ptr i32) (result i32 i32)
    local.get $ptr
    i32.load8_u
    call $dup

    i32.const 1
    i32.shr_u ;; size

    i32.const 1
    i32.and ;; free
  )

  ;; UTILS
  (func $dup (param $a i32) (result i32 i32)
    local.get $a
    local.get $a
  )

  ;; TODO

  (func $malloc (export "malloc") (param $size i32) (result i32)
    (local $block_size i32)  ;; 0
    (local $block_free i32)  ;; 0
    call $decode
    local.set $block_size
    local.set $block_free

    local.get $block_size
    local.get $size
    i32.ge_u

    i32.const 0
  )

)
