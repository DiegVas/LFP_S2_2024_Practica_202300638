module generateInfo
   use equipment_type

contains
   subroutine generar_informe_inventario(equipment_List, numEquipments)
      implicit none
      type(Equipment), dimension(:), intent(in) :: equipment_List
      integer, intent(in) :: numEquipments
      integer :: i, unit_number, iostat_val
      character(len=50) :: fileName
      real :: valorTotal

      fileName = 'informe.txt'
      unit_number = 10
      open (unit_number, file=fileName, status='replace', action='write', iostat=iostat_val)
      if (iostat_val /= 0) then
         print *, 'Error al abrir el archivo para escribir el informe.'
         return
      end if

      write (unit_number, '(A)') 'Informe de Inventario'
      write (unit_number, '(A)') 'Equipo      Cantidad   Precio Unitario   Valor Total   Ubicación'
      write (unit_number, '(A)') '---------------------------------------------------------------'

      do i = 1, numEquipments
         valorTotal = equipment_List(i)%Quantity*equipment_List(i)%price
         write (unit_number, '(A, I10, F12.2, F12.2, A)') trim(equipment_List(i)%Name), &
            equipment_List(i)%Quantity, equipment_List(i)%Price, valorTotal, trim(equipment_List(i)%Ubication)
      end do

      close (unit_number)

   end subroutine generar_informe_inventario

end module generateInfo
