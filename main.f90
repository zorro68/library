
    
    PROGRAM LIB
    
    USE MOD_FACT
    !USE MOD_STRINGS
    
    INTEGER(4) :: IRES, IVAR=8
    INTEGER(8) :: IIRES, IIVAR=8
    REAL(4) :: SRES, SVAR=8.0
    REAL(8) :: DRES, DVAR=8.D0
    REAL(16) :: QRES, QVAR=8.Q0
    REAL(4) :: SSRES
    REAL(8)::DSRES
    REAL(16)::QSRES
    
    REAL*8 DVAL, DVAL1,DGAMMA
    REAL*16 QVAL,QVAL1,QGAMMA

    IRES=FACT(IVAR)
    WRITE(*,*),IVAR,"    ",IRES
    IIRES=FACT(IIVAR)
    WRITE(*,*)IIVAR,"     ",IIRES
    SRES=FACT(SVAR)
    WRITE(*,*)PRECISION(SVAR),"     ",SVAR,"    ",PRECISION(FACT(SVAR)),"     ",SRES
    DRES=FACT(DVAR)
    WRITE(*,*)PRECISION(DVAR),"     ",DVAR,"    ",PRECISION(FACT(DVAR)),"     ",DRES
    QRES=FACT(QVAR)
    WRITE(*,*)PRECISION(QVAR),"     ",QVAR,"    ",PRECISION(FACT(QVAR)),"     ",QRES
    SSRES=SEMIFACT(SVAR)
    WRITE(*,*)PRECISION(SVAR),"     ",SVAR,"    ",PRECISION(SEMIFACT(SVAR)),"     ",SSRES
    DSRES=SEMIFACT(DVAR)
    WRITE(*,*)PRECISION(DVAR),"     ",DVAR,"    ",PRECISION(SEMIFACT(DVAR)),"     ",DSRES
    QSRES=SEMIFACT(QVAR)
    WRITE(*,*)PRECISION(QVAR),"     ",QVAR,"    ",PRECISION(SEMIFACT(QVAR)),"     ",QSRES
    
    WRITE(*,*)'**********************************************************************'
     
    !CON ESTO DEMUESTRO QUE NO HAY QUE USAR DSQRT O QSQRT YA QUE SQRT SE ADAPTA AL ARGUMENTO
    WRITE(*,*)SVAR,SQRT(SVAR),PRECISION(SQRT(SVAR))
    WRITE(*,*)DVAR,SQRT(DVAR),PRECISION(SQRT(DVAR))
    WRITE(*,*)QVAR,SQRT(QVAR),PRECISION(SQRT(QVAR))
    
    WRITE(*,*)'**********************************************************************'
    
   
    
    PAUSE
    
    END