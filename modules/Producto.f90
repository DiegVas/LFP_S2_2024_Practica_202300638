module producto
   use readFiles
   implicit none
   type :: productos
      character(len=100) :: nombre
      integer :: cantidad
      real :: precio
      character(len=100) :: bodega
   contains
      procedure :: mostrarProducto
   end type productos

contains
   subroutine mostrarProducto(this)
      implicit none
      class(productos), intent(in) :: this
      print *, "Nombre: ", this%nombre
      print *, "Cantidad: ", this%cantidad
      print *, "Precio: ", this%precio
   end subroutine mostrarProducto

end module producto
