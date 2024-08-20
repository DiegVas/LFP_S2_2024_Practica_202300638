
![fiusac](https://github.com/user-attachments/assets/8ef01dc2-18fa-40ca-ab60-7b1b61778c3c)

# LENGUAJES FORMALES Y DE PROGRAMACIÓN - SEGUNDO SEMESTRE 2024

### Este proyecto es un sistema de gestión de inventarios que permite cargar inventarios iniciales, procesar instrucciones de movimientos y generar informes de inventario. Está desarrollado en Fortran y consta de varios módulos que se encargan de diferentes funcionalidades.

- ## Menu
  ![Menu](https://github.com/user-attachments/assets/90178f08-f461-4763-b59e-d8b802acaaec)

  #### Al inicar el programa se le mostrara el siguiente menu el cual podra ingresar la opcion que usted quiera utilizar.
  `👀 Observaciones `: Antes de usar las opciones 2 y 3 es obligatorio el seleccionar con anteoridad la opcion 1 la cual es para cargar el inventario
  - ### Opcion 1 Cargar Inventario
    ![Opcion 1](https://github.com/user-attachments/assets/d1e53eef-617f-4029-aec3-e022b5bdb16d)
    #### Aqui deverá ingresar el nombre del archivo **.inv** el cual tendra el inventario a gestionar
    ![cargar](https://github.com/user-attachments/assets/5f76a6a8-c676-4668-8cb5-79ba905f79a9)
    #### Si todo salio sin ningun error entonces aparecera este mensaje el cual indica que el inventario fue cargado correctamente en el programa

  - ### Opcion 2 Cargar Instrucciones
  ![instrucciones](https://github.com/user-attachments/assets/e7260816-bf08-4b6f-8c9c-6ab1bb28fa25)
  #### Mismo proceso que en anterior ingrese el nombre del archivo **.mov** el cual tendra las intrucciones a realizar en el inventario
  #### Existen dos tipos de errores  uno de ellos es al momento de "eliminar_equipo" si este exede la cantidad del stock disponible este mostrara un error:
  ![Error 1](https://github.com/user-attachments/assets/f546edaf-c890-4944-9217-02cdd5bd6b9a)
  #### Otro error es el de no encontrar el equipo para gestionar la instruccion.

  - ### Opcion 3 Generar Informes
  #### Una vez realizado todas las gestiones deseadas podremos generar nuestro informe del inventario el cual sera gaurdado en la carpeta ** build **
  ![Informe  ](https://github.com/user-attachments/assets/a5981f94-072c-499f-9487-f2cf6e7ee1c5)
