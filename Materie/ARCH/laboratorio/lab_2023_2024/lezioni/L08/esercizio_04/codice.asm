.globl _start

.data
	v: .word 1, 2, 3, 4
	s: .word 4

.text

_start:
	la a0, v,
	la a1, s
	lw a1, 0(a1)
	
	jal ra, invert
	
	beq zero, zero, exit
################################################################################
# a0  -> address of v
# a1 -> size of v
################################################################################	
invert:
	addi sp, sp, -24
	li a2, 0
	sd ra, 0(sp)
	sw s1, 8(sp)
	sw s2, 12(sp)
	sd a0, 16(sp)
	
	add s1, zero, a1	# i = size(v[]) - 1
	li s2, 0		# j = 0
	
	begin_loop:
	beq s1, s2, end_loop
		ld a0, 16(sp)
		add a1, zero, s1
		add a2, zero, s2
		jal ra, swap
		
		addi s1, s1, -1
		addi s2, s2, 1
		beq zero, zero, begin_loop
	end_loop:
	
	ld ra, 0(sp)
	ld s1, 8(sp)
	ld s2, 12(sp)
	addi sp, sp, 24
	jr ra

		
	
################################################################################
# Procedure swap(v, x, y)
# a0  -> address of v
# a1 -> index x
# a2 -> index y
# does not return anything
# swap is a leaf procedure that can be implemented with only temp registers
################################################################################
swap:
      slli  a1, a1, 2     # calculates offset of x
      slli  a2, a2, 2     # calculates offset of y
      add   t0, a0, a1    # address of v[x]
      add   t1, a0, a2    # address of v[y]

      lw    t2, 0(t0)     # sway the values
      lw    t3, 0(t1)
      sw    t3, 0(t0)
      sw    t2, 0(t1)
      ret                 # return
exit:
    addi x17, x0, 10  # call number 10 = exit
    ecall
