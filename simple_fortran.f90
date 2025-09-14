! Simplified version that just shows the calling convention
subroutine test_dgemm_simple(C)
  real(8), dimension(3,3), intent(out) :: C
  real(8), dimension(3,3) :: A, B
  
  ! Initialize A with simple values
  A = reshape([1.0d0, 4.0d0, 7.0d0, &
               2.0d0, 5.0d0, 8.0d0, &
               3.0d0, 6.0d0, 9.0d0], [3,3])
  
  ! B as identity
  B = 0.0d0
  B(1,1) = 1.0d0; B(2,2) = 1.0d0; B(3,3) = 1.0d0
  
  ! Call BLAS
  call dgemm('N', 'N', 3, 3, 3, 1.0d0, A, 3, B, 3, 0.0d0, C, 3)
end subroutine