module equipment_type
   implicit none
   type :: Equipment
      character(len=100) :: name
      integer :: quantity
      real :: price
      character(len=100) :: ubication
   contains
      procedure :: mostrarProducto
   end type Equipment

contains
   subroutine mostrarProducto(this)
      implicit none
      class(equipment), intent(in) :: this
      print *, "Nombre: ", this%name
      print *, "Cantidad: ", this%quantity
      print *, "Precio: ", this%price
   end subroutine mostrarProducto

end module equipment_type
