module Producto
   implicit none
   type :: producto
      character(len=100) :: nombre
      integer :: cantidad
      real :: precio
      character(len=100) :: bodega
   contains
      procedure :: mostrarProducto
   end type producto

contains
   subroutine mostrarProducto(this)
      implicit none
      class(producto), intent(in) :: this
      print *, "Nombre: ", this%nombre
      print *, "Cantidad: ", this%cantidad
      print *, "Precio: ", this%precio
   end subroutine mostrarProducto

end module Producto
