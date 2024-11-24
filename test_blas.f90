program test_blas
  implicit none
  real(8), dimension(4, 4) :: A
  real(8), dimension(2, 2) :: B
  real(8) :: alpha, beta
  integer :: lda, ldb, ldc, m, n, k

  ! Initialize a 4x4 matrix A and a 2x2 matrix B
  A = reshape([ &
       1.0d0, 2.0d0, 3.0d0, 4.0d0, &
       5.0d0, 6.0d0, 7.0d0, 8.0d0, &
       9.0d0, 10.0d0, 11.0d0, 12.0d0, &
       13.0d0, 14.0d0, 15.0d0, 16.0d0 &
       ], shape(A))

  B = reshape([ &
       2.0d0, 1.0d0, &
       0.0d0, 2.0d0 &
       ], shape(B))  ! 2x2 matrix with non-trivial values

  alpha = 1.0d0
  beta = 0.0d0
  lda = 2  ! Leading dimension for submatrix
  ldb = 2  ! Leading dimension of B
  ldc = 2  ! Leading dimension for temporary result
  m = 2    ! Rows in submatrix of A
  n = 2    ! Columns in submatrix of A
  k = 2    ! Inner dimension (columns of A, rows of B)

  print *, "Original Matrix A:"
  call print_matrix(A, 4, 4)

  ! Perform matrix multiplication write to a diffrent UNUSED part of A
  print *, "Updating submatrix A(1:2, 1:2) in place using DGEMM (via TempA):"
  call dgemm('N', 'N', m, n, k, alpha, A(1:2, 1:2), lda, B, ldb, beta, A(3:4, 3:4), ldc)
  call print_parameters('N', 'N', m, n, k, alpha, lda, ldb, beta, ldc)


  print *, "Updated Matrix A:"
  call print_matrix(A, 4, 4)

contains

  subroutine print_matrix(mat, rows, cols)
    real(8), intent(in) :: mat(:, :)
    integer, intent(in) :: rows, cols
    integer :: i, j
    do i = 1, rows
      do j = 1, cols
        write(*, '(F8.2)', advance='no') mat(i, j)
      end do
      print *, ""
    end do
  end subroutine print_matrix

  subroutine print_parameters(transa, transb, m, n, k, alpha, lda, ldb, beta, ldc)
    character(len=1), intent(in) :: transa, transb
    integer, intent(in) :: m, n, k, lda, ldb, ldc
    real(8), intent(in) :: alpha, beta
    print *, "Operation Parameters:"
    print *, "  TRANSA:", transa, "  TRANSB:", transb
    print *, "  M:", m, "  N:", n, "  K:", k
    print *, "  ALPHA:", alpha
    print *, "  LDA:", lda, "  LDB:", ldb, "  LDC:", ldc
    print *, "  BETA:", beta
  end subroutine print_parameters

end program test_blas
