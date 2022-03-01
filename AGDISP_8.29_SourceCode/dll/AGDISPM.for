C AGDISPcalcMeteorology
C Continuum Dynamics, Inc.
C SCIPUFF Implementation: 8.27 03/28/12
C
      SUBROUTINE AGDISPcalcMeteorology(U,V,W)
C
C PURPOSE: recover SCIPUFF met data at droplet location
C
      USE struct_fd
C
      TYPE (material_str) :: SCImat
      TYPE (met_str) :: SCImet
C
      COMMON /SCIM/ SCImat,SCImet
C
      U=SCImet%U
      V=SCImet%V
      W=SCImet%W
C
      RETURN
      END