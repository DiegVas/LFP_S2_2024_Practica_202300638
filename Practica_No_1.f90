program PracticaNO_1

    implicit none
    ! Declaracion de variables

    integer :: option
    character(len=:), allocatable :: contenido


    ! Mostrar Menu
    do while (option /= 4)
        print *, "---------------------------------------------------"
        print *, "Practica 1 - Lenguajes Formales y de programacion"
        print *, "---------------------------------------------------"
        print *, "MENU"
        print *, "1. Cargar Inventario Inicial"
        print *, "2. Cargar Instrucciones de Movimientos"
        print *, "3. Crear Informe de Inventario"
        print *, "4. Salir"
        print *, ""
        print *, "Ingrese una opcion: "
        read *, option

        select case(option)
            case(1)
                contenido = loadInventory()
                print *, contenido
            case(2)
                !call resta()
            case(3)
                !call multiplicacion()
            case(4)
                print *, "Saliendo del programa"
            case default
                print *, "Opcion no valida"
        end select
    end do

contains

    ! Función para leer el archivo y devolver su contenido
    function loadInventory() result(contenido)
        implicit none
        character(len=:), allocatable :: contenido
        integer :: unit_number ! Unidad de asignado
        integer :: iostat_val ! Valor de estado de E/S
        character(len=100) :: line ! Almacernar cada línea
        character(len=:), allocatable :: temp ! Almacenar temporalmente el contenido
        integer :: i, longitud, number,pos

        ! Inicializar contenido
        contenido = ''

        ! Abrir archivo
        open(newunit=unit_number, file='inventario.inv', status='old', action='read', iostat=iostat_val)
        if (iostat_val /= 0) then
            ! Error al abrir el archivo
            print*, 'Error al abrir el archivo'
            return
        end if

        ! Leer archivo línea por línea y concatenar
        do
            
            read(unit_number, '(A)', iostat=iostat_val) line
            if (iostat_val /= 0) exit
                do i = 1, len_trim(line)
                if(line(i:i) == '' .or. line(i:i) == ";") then
                    number = number + 1
                    pos = i
                    print *, line(1:i)
                end if
              print *, line(i:i)
            end do
            if (len_trim(contenido) == 0) then
                ! Primera línea
                contenido =trim(line)
                
            else
                ! Siguientes líneas
                temp = contenido
                contenido = trim(temp) // new_line('a') // trim(line)
            end if
        end do

        ! Cerrar archivo
        close(unit_number)
    end function loadInventory


end program PracticaNO_1

