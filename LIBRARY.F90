!!************************************************************************************
!!
!!                      SUBROUTINES AND FUNCTIONS FOR SPECIAL UTILITIES
!!
!!************************************************************************************

    
!MODULE PREC
!    USE, INTRINSIC :: ISO_FORTRAN_ENV, ONLY : INT32, INT64, REAL32, REAL64, REAL128 
!    
!    IMPLICIT NONE
!    PRIVATE
!    INTEGER, PARAMETER :: I4 = INT32, I8 = INT64
!    INTEGER, PARAMETER :: SP= REAL32, DP = REAL64, QP = REAL128
!    !INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37), dp = SELECTED_REAL_KIND(15,307), qp = SELECTED_REAL_KIND(33,4931)
!    
!    PUBLIC :: I4, I8, SP, DP, QP
!END MODULE PREC

MODULE CONST
    IMPLICIT NONE
    PRIVATE
    INTEGER, PARAMETER :: LONG=300
    
    PUBLIC :: LONG
END MODULE

    
!!====================================================================================
!!                     FUNCTIONS RELATED TO THE FACTORIAL FUNCTION
!!====================================================================================


MODULE MOD_FACT
    !USE PREC
    IMPLICIT NONE
    
    PUBLIC FACT, FACTI, SEMIFACT
    PRIVATE I4FACT,I8FACT,SFACT,DFACT,QFACT
    PRIVATE I4FACTI,I8FACTI,SFACTI,DFACTI,QFACTI
    PRIVATE SSFACT,DSFACT,QSFACT

    INTERFACE FACT
        MODULE PROCEDURE I4FACT,I8FACT,SFACT,DFACT,QFACT
    END INTERFACE

    INTERFACE FACTI
        MODULE PROCEDURE I4FACTI,I8FACTI,SFACTI,DFACTI,QFACTI
    END INTERFACE

    INTERFACE SEMIFACT
        MODULE PROCEDURE SSFACT, DSFACT, QSFACT
    END INTERFACE
    CONTAINS 
     
! --------------------------------------------------
!   FUNCTION TO CALCULATE FACTORIAL FUNCTION OF NUM
! --------------------------------------------------

    FUNCTION I4FACT(NUM)
        INTEGER(4), INTENT(IN) :: NUM
        INTEGER(4)::I4FACT
	    INTEGER(4) ::J
	
	    I4FACT=1
	    IF(NUM==0)RETURN
	    DO J=1,NUM
		    I4FACT=I4FACT*J
	    END DO
    END FUNCTION
    
    FUNCTION I8FACT(NUM)
        INTEGER(8), INTENT(IN) :: NUM
        INTEGER(8)::I8FACT
	    INTEGER(8):: J
	
	    I8FACT=1
	    IF(NUM==0)RETURN
	    DO J=1,NUM
		    I8FACT=I8FACT*J
	    END DO
    END FUNCTION
    
    FUNCTION SFACT(SNUM)
        REAL(4), INTENT(IN) :: SNUM
        REAL(4)::SFACT
        INTEGER(4) NUM,J
        
        NUM=INT(SNUM,4)
        SFACT=1.0
        IF(NUM==0)RETURN
        DO J=1,NUM
	        SFACT=SFACT*J
        END DO
    END FUNCTION SFACT
    
    FUNCTION DFACT(DNUM)
        REAL(8), INTENT(IN) :: DNUM
        REAL(8)::DFACT
        INTEGER(4) NUM,J
        
        NUM=INT(DNUM,4)
        DFACT=1.0D0
        IF(NUM==0)RETURN
        DO J=1,NUM
	        DFACT=DFACT*J
        END DO
    END FUNCTION DFACT

    FUNCTION QFACT(QNUM)
        REAL(16), INTENT(IN) :: QNUM
        REAL(16)::QFACT
        INTEGER(8) NUM,J
        
        NUM=INT(QNUM,8)
        QFACT=1.0Q0
        IF(NUM==0)RETURN
	    DO J=1,NUM
		    QFACT=QFACT*J
	    END DO
    END FUNCTION

! --------------------------------------------------------------------
!   FUNCTION TO CALCULATE THE DOUBLE FACTORIAL FUNCTION OF A ODD NUMBER (NUM)
! --------------------------------------------------------------------

    FUNCTION I4FACTI(NUM)
        INTEGER(4), INTENT(IN) :: NUM
        INTEGER(4)::I4FACTI
	    INTEGER(4)::J
	
        IF(MOD(NUM,2)==0) STOP 'ERROR IN FACTI_I4: THE NUMBER IS NO AN ODD ONE'
	    
        I4FACTI=1 
        IF(NUM<=0)RETURN
	    DO J=1,NUM,2
		    I4FACTI=I4FACTI*J
	    END DO
    END FUNCTION

    FUNCTION I8FACTI(NUM)
        INTEGER(8), INTENT(IN) :: NUM
        INTEGER(8)::I8FACTI
	    INTEGER(8)::J
	
        IF(MOD(NUM,2)==0) STOP 'ERROR IN FACTI_I8: THE NUMBER IS NO AN ODD ONE'
	    
        I8FACTI=1
        IF(NUM<=0)RETURN
	    DO J=1,NUM,2
		    I8FACTI=I8FACTI*J
	    END DO
    END FUNCTION
    
    FUNCTION SFACTI(SNUM)
        REAL(4), INTENT(IN) :: SNUM
        REAL(4)::SFACTI
        INTEGER(4)::J,NUM
        
        NUM=SNUM !TRUNCATE THE SNUM
        IF(MOD(NUM,2)==0) STOP 'ERROR IN SFACTI: THE NUMBER IS NO AN ODD ONE'
        
        SFACTI=1.0 
        IF(NUM<=0)RETURN
	    DO J=1,NUM,2
		    SFACTI=SFACTI*REAL(J,4)
	    END DO
    END FUNCTION
    
    FUNCTION DFACTI(DNUM)
        REAL(8), INTENT(IN) :: DNUM
        REAL(8)::DFACTI
        INTEGER(4)::J,NUM
        
        NUM=DNUM !TRUNCATE THE DNUM
        IF(MOD(NUM,2)==0) STOP 'ERROR IN DFACTI: THE NUMBER IS NO AN ODD ONE'
        
        DFACTI=1.D0 
        IF(NUM<=0)RETURN
	    DO J=1,NUM,2
		    DFACTI=DFACTI*REAL(J,8)
	    END DO
    END FUNCTION
    
    FUNCTION QFACTI(QNUM)
        REAL(16), INTENT(IN) :: QNUM
        REAL(16)::QFACTI
        INTEGER(4)::J,NUM
        
        NUM=QNUM !TRUNCATE THE QNUM
        IF(MOD(NUM,2)==0) STOP 'ERROR IN QFACTI: THE NUMBER IS NO AN ODD ONE'
        
        QFACTI=1.Q0
        IF(NUM<=0)RETURN
	    DO J=1,NUM,2
		    QFACTI=QFACTI*REAL(J,16)
	    END DO
    END FUNCTION
    
! ----------------------------------------------------------
!   FUNCTION TO CALCULATE POSITIVE SEMI-FACTORIAL (NUM+1/2)!
! ----------------------------------------------------------
    FUNCTION SSFACT(NUM)
        REAL(4),INTENT(IN):: NUM
        REAL(4)::SSFACT,SQRT
        REAL(4), PARAMETER :: PI_4 = 4.0*ATAN(1.0)
        INTEGER(4)::J
        
	    SSFACT=SQRT(PI_4)/2.0
        IF(NUM==0)RETURN
	    DO J=1,NUM
		    SSFACT=SSFACT*REAL(J+0.5)
	    END DO
	    RETURN
    END
    
    FUNCTION DSFACT(NUM)
        REAL(8),INTENT(IN):: NUM
        REAL(8)::DSFACT,DSQRT
        REAL(8), PARAMETER :: PI_8 = 4.D0*DATAN(1.D0)
        INTEGER(4)::J
        !REAL(8), PARAMETER :: RAIZPI=1.772453850905516D0
        
	    DSFACT=DSQRT(PI_8)/2.D0
        IF(NUM==0)RETURN
	    DO J=1,NUM
		    DSFACT=DSFACT*DBLE(J+0.5D0)
	    END DO
	    !DSFACT=RAIZPI*DFACTI(2*NUM+1)/2.D0**(NUM+1)
        RETURN
    END
    
    FUNCTION QSFACT(NUM)
        REAL(16),INTENT(IN):: NUM
        REAL(16)::QSFACT,QSQRT
        REAL(16), PARAMETER :: PI_16 = 4.Q0*QATAN(1.Q0)
        INTEGER(4)::J 
        !REAL(16), PARAMETER :: RAIZPI=1.772453850905516Q0
    
	    QSFACT=QSQRT(PI_16)/2.Q0
        IF(NUM==0)RETURN
	    DO J=1,NUM
		    QSFACT=QSFACT*(REAL(J,16)+0.5Q0)
	    END DO
        RETURN
    END

END MODULE MOD_FACT

!!====================================================================================
!!                     FUNCTIONS RELATED TO THE GAMMA FUNCTION
!!====================================================================================

! -------------------------------------------------
!   FUNCTION TO CALCULATE THE GAMMA FUNCTION OF XX 
!   (IT ADMITS NEGATIVE AND POSITIVE VALUE OF XX)
!   NUMERICAL RECIPES PG. 207 (MODIFIED) 
! -------------------------------------------------

REAL*8 FUNCTION DGAMMA(XX)
	IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION COF(6)
	DATA COF,STP/76.18009173D0,-86.50532033D0,24.01409822D0,-1.231739516D0,.120858003D-2,-.536382D-5,2.50662827465D0/
	
	DGAMMA=1.D0
	IF(XX.GT.0.999999999D0.AND.XX.LT.1.000000001D0)RETURN
	IF(XX.GT.1.999999999D0.AND.XX.LT.2.000000001D0)RETURN 

	F=XX

    DO WHILE (F.LT.0.D0)
        DGAMMA=DGAMMA/F
		F=F+1.D0
    ENDDO

	X=F-1.0D0
	TMP=X+5.5D0
	TMP=(X+0.5D0)*DLOG(TMP)-TMP
	SER=1.0D0
	DO J=1,6
		X=X+1.0D0
		SER=SER+COF(J)/X
	END DO
	GLOG=TMP+DLOG(STP*SER)
  
	DGAMMA=DGAMMA*DEXP(GLOG)
	
	RETURN
    END
    
REAL*16 FUNCTION QGAMMA(XX)
	IMPLICIT REAL*16(A-H,O-Z)
	DIMENSION COF(6)
	DATA COF,STP/76.18009173Q0,-86.50532033Q0,24.01409822Q0,-1.231739516Q0,.120858003Q-2,-.536382Q-5,2.50662827465Q0/
	
	QGAMMA=1.Q0
	IF(XX.GT.0.999999999Q0.AND.XX.LT.1.000000001Q0)RETURN
	IF(XX.GT.1.999999999Q0.AND.XX.LT.2.000000001Q0)RETURN 

	F=XX

    DO WHILE (F.LT.0.Q0)
        QGAMMA=QGAMMA/F
		F=F+1.Q0
    ENDDO

	X=F-1.0Q0
	TMP=X+5.5Q0
	TMP=(X+0.5Q0)*QLOG(TMP)-TMP
	SER=1.0Q0
	DO J=1,6
		X=X+1.0Q0
		SER=SER+COF(J)/X
	END DO
	GLOG=TMP+QLOG(STP*SER)
  
	QGAMMA=QGAMMA*QEXP(GLOG)
	
	RETURN
END

REAL*8 FUNCTION DGAMI(A,X)
    IMPLICIT NONE
    REAL*8 A,X,DGAMMA,DGAMMAP
    DGAMI=DGAMMA(A)*DGAMMAP(A,X)
    RETURN
    END    
    
REAL*16 FUNCTION QGAMI(A,X)
    IMPLICIT NONE
    REAL*16 A,X,QGAMMA,QGAMMAP
    QGAMI=QGAMMA(A)*QGAMMAP(A,X)
    RETURN
END  

REAL*8 FUNCTION DGAMIC(A,X)
    IMPLICIT NONE
    REAL*8 A,X,DGAMMA,DGAMMAP
    DGAMIC=DGAMMA(A)*(1.D0-DGAMMAP(A,X))
    RETURN
END
    
REAL*16 FUNCTION QGAMIC(A,X)
    IMPLICIT NONE
    REAL*16 A,X,QGAMMA,QGAMMAP
    QGAMIC=QGAMMA(A)*(1.Q0-QGAMMAP(A,X))
    RETURN
END

! -------------------------------------------------
!   FUNCTION TO CALCULATE THE INCOMPLETE GAMMA   
!   FUNCTION P(a,x).
!   NUMERICAL RECIPES PG. 211 (MODIFIED)
! -------------------------------------------------

REAL*8 FUNCTION DGAMMAP(A,X)
    IMPLICIT NONE
    REAL*8 A,X,DGSER,DGCF
    
	IF(X<0.D0.OR.A<=0.D0)PAUSE 'BAD ARGUMENTS IN GAMMAP'
    IF(X<A+1.D0)THEN
		DGAMMAP=DGSER(A,X)
	ELSE
		DGAMMAP=1.D0-DGCF(A,X)
	ENDIF
    END
    
REAL*16 FUNCTION QGAMMAP(A,X)
    IMPLICIT NONE
    REAL*16 A,X,QGSER,QGCF
    
	IF(X<0.Q0.OR.A<=0.Q0)PAUSE 'BAD ARGUMENTS IN GAMMAP'
    IF(X<A+1.Q0)THEN
		QGAMMAP=QGSER(A,X)
	ELSE
		QGAMMAP=1.Q0-QGCF(A,X)
	ENDIF
    END

REAL*8 FUNCTION DGAMMAQ(A,X)
    IMPLICIT NONE
    REAL*8 A,X,DGSER,DGCF,DGAMMQ
    
    IF(X<0.D0.OR.A<=0.D0)PAUSE 'BAD ARGUMENTS IN GAMMAQ'
    IF(X<A+1.D0)THEN
        DGAMMAQ=1.D0-DGSER(A,X)
    ELSE
        DGAMMQ=DGCF(A,X)
    ENDIF
    END
    
REAL*16 FUNCTION QGAMMAQ(A,X)
    IMPLICIT NONE
    REAL*16 A,X,QGSER,QGCF,QGAMMQ
    
    IF(X<0.Q0.OR.A<=0.Q0)PAUSE 'BAD ARGUMENTS IN GAMMAQ'
    IF(X<A+1.Q0)THEN
        QGAMMAQ=1.Q0-QGSER(A,X)
    ELSE
        QGAMMQ=QGCF(A,X)
    ENDIF
END
    
! -------------------------------------------------
!   FUNCTION TO CALCULATE THE INCOMPLETE GAMMA   
!   FUNCTION P(a,x), EVALUATED BY ITS SERIES 
!   REPRESENTATION.
!   NUMERICAL RECIPES PG. 212 (MODIFIED)
! -------------------------------------------------

REAL*8 FUNCTION DGSER(A,X)
    IMPLICIT NONE
    INTEGER*2 ITMAX
    REAL*8 A,X,EPS
    PARAMETER (ITMAX=100,EPS=3.D-7)
    INTEGER*2 N
    REAL*8 AP,DEL,SUM,DGAMMA
    
	IF(X==0.D0)THEN
		DGSER=0.D0
		RETURN
	ENDIF

	AP=A
	SUM=1.D0/A
	DEL=SUM
	DO N=1,ITMAX
		AP=AP+1.D0
		DEL=DEL*X/AP
		SUM=SUM+DEL
		IF(DABS(DEL)<DABS(SUM)*EPS)EXIT
	ENDDO
	IF(N>ITMAX)PAUSE 'A TOO LARGE, ITMAX TOO SMALL IN GSER'
	DGSER=SUM*DEXP(-X+A*DLOG(X)-DLOG(DGAMMA(A)))
END

REAL*16 FUNCTION QGSER(A,X)
    IMPLICIT NONE
    INTEGER*2 ITMAX
    REAL*16 A,X,EPS
    PARAMETER (ITMAX=100,EPS=3.Q-7)
    INTEGER*2 N
    REAL*16 AP,DEL,ADD,QGAMMA
    
	IF(X==0.Q0)THEN
		QGSER=0.Q0
		RETURN
	ENDIF

	AP=A
	ADD=1.Q0/A
	DEL=ADD
	DO N=1,ITMAX
		AP=AP+1.Q0
		DEL=DEL*X/AP
		ADD=ADD+DEL
		IF(QABS(DEL)<QABS(ADD)*EPS)EXIT
	ENDDO
	IF(N>ITMAX)PAUSE 'A TOO LARGE, ITMAX TOO SMALL IN GSER'
	QGSER=ADD*QEXP(-X+A*QLOG(X)-QLOG(QGAMMA(A)))
END

! -------------------------------------------------
!   FUNCTION TO CALCULATE THE INCOMPLETE GAMMA   
!   FUNCTION P(a,x), EVALUATED BY ITS CONTINUED 
!   FRACTION REPRESENTATION.
!   NUMERICAL RECIPES PG. 212 (MODIFIED)
! -------------------------------------------------

REAL*8 FUNCTION DGCF(A,X)
    IMPLICIT NONE
    INTEGER*2 ITMAX,I
    REAL*8 A,X,EPS,FPMIN
    PARAMETER (ITMAX=100,EPS=3.D-7,FPMIN=1.D-30)
	REAL*8 AN,B,C,D,DEL,H,DGAMMA

    IF(X==0.D0) then
        DGCF=1.D0
        RETURN
    ENDIF

   	B=X+1.D0-A
	C=1.D0/FPMIN
	D=1.D0/B
	H=D
	DO I=1,ITMAX
		AN=-I*(I-A)
		B=B+2.D0
		D=AN*D+B
		IF(DABS(D)<FPMIN)D=FPMIN
		C=B+AN/C
		IF(DABS(C)<FPMIN)C=FPMIN
		D=1.D0/D
		DEL=D*C
		H=H*DEL
		IF(DABS(DEL-1.D0)<EPS)EXIT
	ENDDO
	IF(I>ITMAX)PAUSE 'A TOO LARGE, ITMAX TOO SMALL IN GCF'
	DGCF=DEXP(-X+A*DLOG(X)-DLOG(DGAMMA(A)))*H
    END
    
REAL*16 FUNCTION QGCF(A,X)
    IMPLICIT NONE
    INTEGER*2 ITMAX,I
    REAL*16 A,X,EPS,FPMIN
    PARAMETER (ITMAX=100,EPS=3.E-7,FPMIN=1.E-30)
	REAL*16 AN,B,C,D,DEL,H,QGAMMA

    IF(X==0.Q0) then
        QGCF=1.Q0
        RETURN
    ENDIF

   	B=X+1.Q0-A
	C=1.Q0/FPMIN
	D=1.Q0/B
	H=D
	DO I=1,ITMAX
		AN=-I*(I-A)
		B=B+2.Q0
		D=AN*D+B
		IF(QABS(D)<FPMIN)D=FPMIN
		C=B+AN/C
		IF(QABS(C)<FPMIN)C=FPMIN
		D=1.Q0/D
		DEL=D*C
		H=H*DEL
		IF(QABS(DEL-1.Q0)<EPS)EXIT
	ENDDO
	IF(I>ITMAX)PAUSE 'A TOO LARGE, ITMAX TOO SMALL IN GCF'
	QGCF=QEXP(-X+A*QLOG(X)-QLOG(QGAMMA(A)))*H
END
 
    
!!====================================================================================
!!                     FUNCTIONS RELATED TO SPECIAL FUNCTIONS
!!====================================================================================

! --------------------------------------------------
!   FUNCTION TO CALCULATE KRONEKER'S DELTA FUNCTION
! --------------------------------------------------

INTEGER*2 FUNCTION DELTAK(N,M)
    IMPLICIT NONE
	INTEGER*2 N,M
	DELTAK=0
	IF (N==M) DELTAK=1
    END

    
!!====================================================================================
!!            SUBROUTINES AND FUNCTIONS RELATED TO MATRIX OPERATIONS
!!====================================================================================

!------------------------------------------------------
!   SUBROUTINE TO WRITE A NxM MATRIX (N=ROWS,M=COLUMNS)
!------------------------------------------------------

SUBROUTINE MATOUT(N,M,A,C,F) !C=TRUE,FALSE TO WRITE OR DONT THE CLASP, F=0 ON SCREEN,<>0 ON FILE
    IMPLICIT NONE
	INTEGER*4 :: N,M,F,I,J
	REAL*8 :: A(1:N,1:M)
	LOGICAL :: C
	
	IF (F==0)THEN
		IF(C==.TRUE.) THEN
			WRITE(*,'(A1)',ADVANCE='NO')CHAR(218)
			DO I=1,M
				WRITE(*,'(A20)',ADVANCE='NO')'     '
			ENDDO
			WRITE(*,'(A1,A1)',ADVANCE='YES')' ',CHAR(191)
		ENDIF

		DO I=1,N
			IF(C==.TRUE.) WRITE(*,'(A1)',ADVANCE='NO')CHAR(179)
			DO J=1,M
				WRITE(*,'(F20.13)',ADVANCE='NO')A(I,J)
			ENDDO
			IF(C==.TRUE.) WRITE(*,'(A1,A1)',ADVANCE='YES')' ',CHAR(179)
			IF(C==.FALSE.)WRITE(*,'(A1,A1)',ADVANCE='YES')' '
		ENDDO

		IF(C==.TRUE.) THEN
			WRITE(*,'(A1)',ADVANCE='NO')CHAR(192)
			DO I=1,M
				WRITE(*,'(A20)',ADVANCE='NO')'     '
			ENDDO
			WRITE(*,'(A1,A1)',ADVANCE='YES')' ',CHAR(217)
		ENDIF
	ELSE
		IF(C==.TRUE.) THEN
			WRITE(F,'(A1)',ADVANCE='NO')CHAR(218)
			DO I=1,M
				WRITE(F,'(A20)',ADVANCE='NO')'     '
			ENDDO
			WRITE(F,'(A1,A1)',ADVANCE='YES')' ',CHAR(191)
		ENDIF

		DO I=1,N
			IF(C==.TRUE.) WRITE(F,'(A1)',ADVANCE='NO')CHAR(179)
			DO J=1,M
				WRITE(F,'(F20.13)',ADVANCE='NO')A(I,J)
			ENDDO
			IF(C==.TRUE.) WRITE(F,'(A1,A1)',ADVANCE='YES')' ',CHAR(179)
			IF(C==.FALSE.)WRITE(F,'(A1,A1)',ADVANCE='YES')' '
		ENDDO

		IF(C==.TRUE.) THEN
			WRITE(F,'(A1)',ADVANCE='NO')CHAR(192)
			DO I=1,M
				WRITE(F,'(A20)',ADVANCE='NO')'     '
			ENDDO
			WRITE(F,'(A1,A1)',ADVANCE='YES')' ',CHAR(217)
		ENDIF
	ENDIF
END

! -------------------------------------------------
!   SUBROUTINE TO CALCULATE SS^(-1) (SS=NxN SYMMETRIC MATRIX)
! -------------------------------------------------

SUBROUTINE DMATINVSYM(N,SS,SR)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INTEGER*4 N,I,J
	REAL*8 TOL
	REAL*8 SS(1:N,1:N),SR(1:N,1:N),TT(1:N,1:N),AUX(1:N,1:N)
	LOGICAL MASK(1:N,1:N)

	TOL=1.D-15

	FORALL(I=1:N,J=1:N) SR(I,J)=0.D0
	FORALL(I=1:N,J=1:N) TT(I,J)=0.D0
	FORALL(I=1:N,J=1:N) AUX(I,J)=SS(I,J)
	
	CALL DJACOBI(N,AUX,TT,0)
    !CALL JACOBIM(N,AUX,TT)

	MASK=DABS(AUX)<TOL	!ELEMENTS = 0 WHEN ELEMENTS < TOL
	AUX=MERGE(AUX,SR,.NOT.MASK)

    DO I=1,N
		IF(DABS(AUX(I,I))<TOL)AUX(I,I)=TOL*DBLE(I)
		AUX(I,I)=1.D0/AUX(I,I)
	END DO

	SR=MATMUL(TT,MATMUL(AUX,TRANSPOSE(TT)))

    RETURN
    END
        
SUBROUTINE QMATINVSYM(N,SS,SR)
    IMPLICIT REAL*16 (A-H,O-Z)
	INTEGER*4 N,I,J
	REAL*16 TOL
	REAL*16 SS(1:N,1:N),SR(1:N,1:N),TT(1:N,1:N),AUX(1:N,1:N)
	LOGICAL MASK(1:N,1:N)

	TOL=1.Q-15

	FORALL(I=1:N,J=1:N) SR(I,J)=0.Q0
	FORALL(I=1:N,J=1:N) TT(I,J)=0.Q0
	FORALL(I=1:N,J=1:N) AUX(I,J)=SS(I,J)
	
	CALL QJACOBI(N,AUX,TT,0)

	MASK=QABS(AUX)<TOL	!ELEMENTS = 0 WHEN ELEMENTS < TOL
	AUX=MERGE(AUX,SR,.NOT.MASK)

    DO I=1,N
		IF(QABS(AUX(I,I))<TOL)AUX(I,I)=TOL*REAL(I,16)
		AUX(I,I)=1.Q0/AUX(I,I)
	END DO

	SR=MATMUL(TT,MATMUL(AUX,TRANSPOSE(TT)))

    RETURN
END
    
! --------------------------------------------------------------
!   SUBROUTINE TO CALCULATE SS^(-1/2) (SS=NxN SYMMETRIC MATRIX) 
! --------------------------------------------------------------

SUBROUTINE DRMATINVSYM(N,SS,SR)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INTEGER*4 N,I,J
	REAL*8 TOL
	REAL*8 SS(1:N,1:N),SR(1:N,1:N),TT(1:N,1:N),AUX(1:N,1:N)
	LOGICAL MASK(1:N,1:N)

	TOL=1.D-15

	FORALL(I=1:N,J=1:N) SR(I,J)=0.D0
	FORALL(I=1:N,J=1:N) TT(I,J)=0.D0
	FORALL(I=1:N,J=1:N) AUX(I,J)=SS(I,J)
	
	CALL DJACOBI(N,AUX,TT,0)
	!CALL DJACOBIM(N,AUX,TT)

	MASK=DABS(AUX)<TOL	!ELEMENTS = 0 WHEN ELEMENTS < TOL
	AUX=MERGE(AUX,SR,.NOT.MASK)

    DO I=1,N
		IF(DABS(AUX(I,I))<TOL)AUX(I,I)=TOL*DBLE(I)
		AUX(I,I)=1.D0/DSQRT(AUX(I,I))
	END DO

	SR=MATMUL(TT,MATMUL(AUX,TRANSPOSE(TT)))

    RETURN
    END
    
SUBROUTINE QMATINVR(N,SS,SR)
    IMPLICIT REAL*16 (A-H,O-Z)
	INTEGER*4 N,I,J
	REAL*16 TOL
	REAL*16 SS(1:N,1:N),SR(1:N,1:N),TT(1:N,1:N),AUX(1:N,1:N)
	LOGICAL MASK(1:N,1:N)

	TOL=1.Q-15

	FORALL(I=1:N,J=1:N) SR(I,J)=0.Q0
	FORALL(I=1:N,J=1:N) TT(I,J)=0.Q0
	FORALL(I=1:N,J=1:N) AUX(I,J)=SS(I,J)
	
	CALL QJACOBI(N,AUX,TT,0)
    !CALL QJACOBIM(N,AUX,TT)

	MASK=QABS(AUX)<TOL	!ELEMENTS = 0 WHEN ELEMENTS < TOL
	AUX=MERGE(AUX,SR,.NOT.MASK)

    DO I=1,N
		IF(QABS(AUX(I,I))<TOL)AUX(I,I)=TOL*REAL(I,16)
		AUX(I,I)=1.Q0/QSQRT(AUX(I,I))
	END DO

	SR=MATMUL(TT,MATMUL(AUX,TRANSPOSE(TT)))

    RETURN
END

! ----------------------------------------------
!   FUNCTION TO CALCULATE THE TRACE OF A MATRIX
! ----------------------------------------------

REAL*8 FUNCTION DTRACE(N,M)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	INTEGER*4 I,N
	REAL*8 M(1:N,1:N)
	
	DTRACE=0.D0
	DO I=1,N
		DTRACE=DTRACE+M(I,I)
	ENDDO
	RETURN
    END
    
REAL*16 FUNCTION QTRACE(N,M)
    IMPLICIT REAL*16 (A-H,O-Z)
	INTEGER*4 I,N
	REAL*16 M(1:N,1:N)
	
	QTRACE=0.Q0
	DO I=1,N
		QTRACE=QTRACE+M(I,I)
	ENDDO
	RETURN
END

! -----------------------------------------------
!   SUBROUTINE TO DIAGONALIZE A SYMMETRIC MATRIX
!   NUMERICAL RECIPES PG. 460 (MODIFIED)
! -----------------------------------------------

SUBROUTINE DJACOBIM(N,A,TT)
    IMPLICIT NONE
	REAL*8 A(1:N,1:N),R(1:N,1:N),TT(1:N,1:N)
    INTEGER*4 N,I,J,K,L,CONT,FLAG
	PARAMETER TOL=1.D-15
	REAL*8 SIGNO,THETA,T,S,C

	FORALL (I=1:N,J=1:N)TT(I,J)=0.D0
	FORALL (I=1:N)TT(I,I)=1.D0

	DO CONT=1,N*N*5 !NUMBER OF ITERATIONS (BETWEEN 3N**2 Y 5N**2)
		DO I=1,N-1
			UNO : DO J=I+1,N
				IF(DABS(A(I,J))<=TOL)EXIT UNO							
				THETA=(A(J,J)-A(I,I))/(2.D0*A(I,J))
				SIGNO=1.D0
				IF(THETA<=0.D0)SIGNO=-1.D0
				T=SIGNO/(DABS(THETA)+DSQRT(THETA*THETA+1.D0))
				!T=-THETA-DSQRT(THETA*THETA+1.D0) !THE SMALLEST SOLUTION
				C=1.D0/DSQRT(1.D0+T*T)
				S=T*C
				FORALL (K=1:N,L=1:N)R(K,L)=0.D0
				FORALL (K=1:N)R(K,K)=1.D0
				R(I,I)=C
				R(J,J)=C
				R(I,J)=S
				R(J,I)=-S
				TT=MATMUL(TT,R)
				A=MATMUL(TRANSPOSE(R),MATMUL(A,R))
				
				FLAG=0
				DO K=1,N
					DO L=K+1,N
						IF(DABS(A(K,L))>TOL)FLAG=1
					ENDDO
				ENDDO
				IF (FLAG==0)RETURN
			ENDDO UNO
		ENDDO
	ENDDO
    END
 
SUBROUTINE QJACOBIM(N,A,TT)
    IMPLICIT REAL*16 (A-H,O-Z)
	REAL*16 A(1:N,1:N),R(1:N,1:N),TT(1:N,1:N)
    INTEGER*4 N,I,J,K,L,CONT,FLAG
	PARAMETER TOL=1.Q-15
	REAL*16 SIGNO,THETA,T,S,C

	FORALL (I=1:N,J=1:N)TT(I,J)=0.Q0
	FORALL (I=1:N)TT(I,I)=1.Q0

	DO CONT=1,N*N*5 !NUMBER OF INTERATIONS (BETWEEN 3N**2 Y 5N**2)
		DO I=1,N-1
			UNO : DO J=I+1,N
				IF(QABS(A(I,J))<=TOL)EXIT UNO							
				THETA=(A(J,J)-A(I,I))/(2.Q0*A(I,J))
				SIGNO=1.Q0
				IF(THETA<=0.Q0)SIGNO=-1.Q0
				T=SIGNO/(QABS(THETA)+QSQRT(THETA*THETA+1.Q0))
				!T=-THETA-QSQRT(THETA*THETA+1.Q0) !THE SMALLEST SOLUTION
				C=1.Q0/QSQRT(1.Q0+T*T)
				S=T*C
				FORALL (K=1:N,L=1:N)R(K,L)=0.Q0
				FORALL (K=1:N)R(K,K)=1.Q0
				R(I,I)=C
				R(J,J)=C
				R(I,J)=S
				R(J,I)=-S
				TT=MATMUL(TT,R)
				A=MATMUL(TRANSPOSE(R),MATMUL(A,R))
				
				FLAG=0
				DO K=1,N
					DO L=K+1,N
						IF(QABS(A(K,L))>TOL)FLAG=1
					ENDDO
				ENDDO
				IF (FLAG==0)RETURN
			ENDDO UNO
		ENDDO
	ENDDO
    END

SUBROUTINE DJACOBI(N,A,T,SORT)
    IMPLICIT DOUBLE PRECISION (A-H,O-Z)
	REAL*8 A(1:N,1:N),T(1:N,1:N)
	PARAMETER TOL=1.D-15
	REAL*8 PR,AMAX,AS,AD,A2,RR,C2,S2,C1,S1,Z1,Z2,T1,T2
	INTEGER*4 N,SORT,I,J,I2,J2,IM


   	FORALL (I=1:N,J=1:N)T(I,J)=0.D0
	FORALL (I=1:N)T(I,I)=1.D0
	
    L=N*N*5
    DO IM=1,L
		AMAX=0.D0
		DO I=2,N
			DO J=1,(I-1)
				IF(AMAX<DABS(A(I,J)))THEN
					AMAX=DABS(A(I,J))
					I2=I
					J2=J
				ENDIF
			ENDDO
		ENDDO
		IF (AMAX<TOL)EXIT
		AS=.5D0*(A(I2,I2)+A(J2,J2))
		AD=.5D0*(A(I2,I2)-A(J2,J2))
		A2=A(I2,J2)
		RR=DSQRT(AD*AD+A2*A2)
		C2=DABS(AD)/RR
		S2=A2/RR
		IF(A(J2,J2)<=A(I2,I2)) S2=-S2
		C1=DSQRT((1.D0+C2)/2.D0)
		S1=.5D0*S2/C1
		DO K=1,N
			Z1=A(K,I2)
			Z2=A(K,J2)
			A(K,I2)=Z1*C1-Z2*S1
			A(I2,K)=A(K,I2)
			A(K,J2)=Z1*S1+Z2*C1
			A(J2,K)=A(K,J2)
		END DO
		A(I2,I2)=AS+AD*C2-A2*S2
		A(J2,J2)=AS-AD*C2+A2*S2
		A(I2,J2)=0.D0
		A(J2,I2)=0.D0
		DO K=1,N
			T1=T(K,I2)
			T2=T(K,J2)
			T(K,I2)=C1*T1-S1*T2
			T(K,J2)=S1*T1+C1*T2
		END DO
	END DO
	
	!SORT THE RESULTS FROM HIGHEST TO LOWEST EIGENVALUE:
	IF(SORT>0)THEN
		DO I=1,N-1
			DO J=I+1,N
				IF(A(I,I)<=A(J,J)) THEN
					AUXI=A(I,I)
					A(I,I)=A(J,J)
					A(J,J)=AUXI
					DO K=1,N
						AUXI=T(K,I)
						T(K,I)=T(K,J)
						T(K,J)=AUXI
					ENDDO
				ENDIF
			ENDDO
		ENDDO
	ENDIF
	!SORT THE RESULTS FROM LOWEST TO HIGHEST EIGENVALUE:
	IF(SORT<0)THEN
		DO I=1,N-1
			DO J=I+1,N
				IF(A(I,I)>=A(J,J)) THEN
					AUXI=A(I,I)
					A(I,I)=A(J,J)
					A(J,J)=AUXI
					DO K=1,N
						AUXI=T(K,I)
						T(K,I)=T(K,J)
						T(K,J)=AUXI
					ENDDO
				ENDIF
			ENDDO
		ENDDO
	ENDIF
END
    
SUBROUTINE QJACOBI(N,A,T,SORT)
    IMPLICIT REAL*16 (A-H,O-Z)
	REAL*16 A(1:N,1:N),T(1:N,1:N)
	PARAMETER TOL=1.Q-15
	REAL*16 PR,AMAX,AS,AD,A2,RR,C2,S2,C1,S1,Z1,Z2,T1,T2
	INTEGER*4 N,SORT,I,J,I2,J2,IM

   	FORALL (I=1:N,J=1:N)T(I,J)=0.Q0
	FORALL (I=1:N)T(I,I)=1.Q0
	
    L=N*N*5
    DO IM=1,L
		AMAX=0.Q0
		DO I=2,N
			DO J=1,(I-1)
				IF(AMAX<QABS(A(I,J)))THEN
					AMAX=QABS(A(I,J))
					I2=I
					J2=J
				ENDIF
			ENDDO
		ENDDO
		IF (AMAX<TOL)EXIT
		AS=.5Q0*(A(I2,I2)+A(J2,J2))
		AD=.5Q0*(A(I2,I2)-A(J2,J2))
		A2=A(I2,J2)
		RR=QSQRT(AD*AD+A2*A2)
		C2=QABS(AD)/RR
		S2=A2/RR
		IF(A(J2,J2)<=A(I2,I2)) S2=-S2
		C1=QSQRT((1.Q0+C2)/2.Q0)
		S1=.5Q0*S2/C1
		DO K=1,N
			Z1=A(K,I2)
			Z2=A(K,J2)
			A(K,I2)=Z1*C1-Z2*S1
			A(I2,K)=A(K,I2)
			A(K,J2)=Z1*S1+Z2*C1
			A(J2,K)=A(K,J2)
		END DO
		A(I2,I2)=AS+AD*C2-A2*S2
		A(J2,J2)=AS-AD*C2+A2*S2
		A(I2,J2)=0.Q0
		A(J2,I2)=0.Q0
		DO K=1,N
			T1=T(K,I2)
			T2=T(K,J2)
			T(K,I2)=C1*T1-S1*T2
			T(K,J2)=S1*T1+C1*T2
		END DO
	END DO
	
	!SORT THE RESULTS FROM HIGHEST TO LOWEST EIGENVALUE:
	IF(SORT>0)THEN
		DO I=1,N-1
			DO J=I+1,N
				IF(A(I,I)<=A(J,J)) THEN
					AUXI=A(I,I)
					A(I,I)=A(J,J)
					A(J,J)=AUXI
					DO K=1,N
						AUXI=T(K,I)
						T(K,I)=T(K,J)
						T(K,J)=AUXI
					ENDDO
				ENDIF
			ENDDO
		ENDDO
	ENDIF
	!SORT THE RESULTS FROM LOWEST TO HIGHEST EIGENVALUE:
	IF(SORT<0)THEN
		DO I=1,N-1
			DO J=I+1,N
				IF(A(I,I)>=A(J,J)) THEN
					AUXI=A(I,I)
					A(I,I)=A(J,J)
					A(J,J)=AUXI
					DO K=1,N
						AUXI=T(K,I)
						T(K,I)=T(K,J)
						T(K,J)=AUXI
					ENDDO
				ENDIF
			ENDDO
		ENDDO
	ENDIF
    END

! -------------------------------------------------------------------
!    SUBROUTINE TO DIAGONALIZE A SYMMETRIC MATRIX (JAIME MODIFICATION)
! -------------------------------------------------------------------
    
SUBROUTINE DJACOBI_JAIME(N,B,X,V,NORD)
!
!	N= MATRIX DIMENSION NxN
!	B= MATRIX TO BE DIAGONALIZED 
!	X= TRANSFORMING MATRIX
!	V= EIGENVECTOR (DIAGONAL OF DIAGONALIZED MATRIX)
!   SI NORD = 1 THE EIGENVALUE ARE NOT SORTED
!
	IMPLICIT REAL*8 (A-H,O-Z)
	PARAMETER (TOL = 1.D-15)
	INTEGER*4 ERROR
	REAL*8 B(1:N,1:N), X(1:N,1:N), V(1:N)
	REAL*8 A(1:N,1:N)
	INTEGER*4 IV(1:N)

	DO I = 1,N
		DO J = I,N
			A(I,J) = B(I,J)
			A(J,I) = B(J,I)
			X(I,J) = 0.D0
			X(J,I) = 0.D0
		ENDDO
		X(I,I) = 1.D0
	ENDDO
    DO J = 1,N
		TOP = 0.D0
        DO I = 1,N
			IF (J/=I) THEN
				AUX = DABS(A(I,J))
				IF (AUX>TOP) THEN
					TOP = AUX
					IV(J) = I
				END IF
			END IF
         END DO
         V(J) = TOP
    END DO

    IT = 0
	TOP=V(1)
	DO WHILE (TOP>=TOL)
		TOP = V(1)
		IQ = 1
		DO I = 2 , N
			IF (V(I)>TOP) THEN
				TOP = V(I)
				IQ = I
			 END IF
		END DO
		IF (TOP<TOL) EXIT
		IP = IV(IQ)
		IF (IP>IQ) THEN
			I = IP
			IP = IQ
			IQ = I
		ENDIF
		AP = A(IP,IP)
		AQ = A(IQ,IQ)
		APQ= A(IP,IQ)
		AL = 2.D0 * APQ
		BE = AQ - AP
		R = DSQRT ( AL*AL + BE*BE )
		C = DSQRT ( 0.5D0*( R + DABS(BE) ) / R )
		S = AL / ( 2.D0 * R * C )
		IF (BE<0.D0 ) S = - S
		DO J = 1,N
			AUX = X(J,IP)*C - X(J,IQ)*S
			X(J,IQ) = X(J,IP)*S + X(J,IQ)*C
			X(J,IP) = AUX
			IF (J/=IP.AND.J/=IQ ) THEN
				AUX = A(J,IP)*C - A(J,IQ)*S
				A(J,IQ) = A(J,IP)*S + A(J,IQ)*C
				A(J,IP) = AUX
				A(IP,J) = AUX
				A(IQ,J) = A(J,IQ)
			ENDIF
		ENDDO
		A(IP,IQ) = 0.D0
		A(IQ,IP) = 0.D0
		AUX = S * ( S*BE - AL*C )
		A(IP,IP) = AP + AUX
		A(IQ,IQ) = AQ - AUX
		IT = IT + 1

		TOP = 0.D0
		DO I = 1,N
			IF (I/=IP) THEN
				AUX = DABS(A(I,IP))
				IF (AUX>TOP) THEN
					TOP = AUX
					IV(IP) = I
				END IF
			END IF
			V(IP) = TOP
		END DO

		TOP = 0.D0
		DO I = 1,N
			IF (I/=IQ) THEN
				AUX = DABS(A(I,IQ))
				IF (AUX>TOP) THEN
					TOP = AUX
					IV(IQ) = I
				END IF
			END IF
			V(IQ) = TOP
		END DO

	ENDDO

    DO I = 1,N
		V(I) = A(I,I)
	ENDDO
!     PRINT*, IT , ' ITERACIONES EN JACOBI2'
	IF (NORD==1) RETURN
	DO I = 1,N
		DO J = I+1,N
			IF (V(J)<V(I)) THEN
				VV=V(J)
				V(J)=V(I)
				V(I)=VV
				DO K = 1,N
					VV=X(K,J)
					X(K,J)=X(K,I)
					X(K,I)=VV
				ENDDO
			ENDIF
		ENDDO
	ENDDO
    RETURN
END
 
SUBROUTINE QJACOBI_JAIME(N,B,X,V,NORD)
!
!	N= MATRIX DIMENSION NxN
!	B= MATRIX TO BE DIAGONALIZED 
!	X= TRANSFORMING MATRIX
!	V= EIGENVECTOR (DIAGONAL OF DIAGONALIZED MATRIX)
!   SI NORD = 1 THE EIGENVALUE ARE NOT SORTED
!
	IMPLICIT REAL*16 (A-H,O-Z)
	PARAMETER (TOL = 1.Q-15)
	INTEGER*4 ERROR
	REAL*16 B(1:N,1:N), X(1:N,1:N), V(1:N)
	REAL*16 A(1:N,1:N)
	INTEGER*4 IV(1:N)

	DO I = 1,N
		DO J = I,N
			A(I,J) = B(I,J)
			A(J,I) = B(J,I)
			X(I,J) = 0.Q0
			X(J,I) = 0.Q0
		ENDDO
		X(I,I) = 1.Q0
	ENDDO
    DO J = 1,N
		TOP = 0.Q0
        DO I = 1,N
			IF (J/=I) THEN
				AUX = QABS(A(I,J))
				IF (AUX>TOP) THEN
					TOP = AUX
					IV(J) = I
				END IF
			END IF
         END DO
         V(J) = TOP
    END DO

    IT = 0
	TOP=V(1)
	DO WHILE (TOP>=TOL)
		TOP = V(1)
		IQ = 1
		DO I = 2 , N
			IF (V(I)>TOP) THEN
				TOP = V(I)
				IQ = I
			 END IF
		END DO
		IF (TOP<TOL) EXIT
		IP = IV(IQ)
		IF (IP>IQ) THEN
			I = IP
			IP = IQ
			IQ = I
		ENDIF
		AP = A(IP,IP)
		AQ = A(IQ,IQ)
		APQ= A(IP,IQ)
		AL = 2.Q0 * APQ
		BE = AQ - AP
		R = QSQRT ( AL*AL + BE*BE )
		C = QSQRT ( 0.5Q0*( R + QABS(BE) ) / R )
		S = AL / ( 2.Q0 * R * C )
		IF (BE<0.Q0 ) S = - S
		DO J = 1,N
			AUX = X(J,IP)*C - X(J,IQ)*S
			X(J,IQ) = X(J,IP)*S + X(J,IQ)*C
			X(J,IP) = AUX
			IF (J/=IP.AND.J/=IQ ) THEN
				AUX = A(J,IP)*C - A(J,IQ)*S
				A(J,IQ) = A(J,IP)*S + A(J,IQ)*C
				A(J,IP) = AUX
				A(IP,J) = AUX
				A(IQ,J) = A(J,IQ)
			ENDIF
		ENDDO
		A(IP,IQ) = 0.Q0
		A(IQ,IP) = 0.Q0
		AUX = S * ( S*BE - AL*C )
		A(IP,IP) = AP + AUX
		A(IQ,IQ) = AQ - AUX
		IT = IT + 1

		TOP = 0.Q0
		DO I = 1,N
			IF (I/=IP) THEN
				AUX = QABS(A(I,IP))
				IF (AUX>TOP) THEN
					TOP = AUX
					IV(IP) = I
				END IF
			END IF
			V(IP) = TOP
		END DO

		TOP = 0.Q0
		DO I = 1,N
			IF (I/=IQ) THEN
				AUX = QABS(A(I,IQ))
				IF (AUX>TOP) THEN
					TOP = AUX
					IV(IQ) = I
				END IF
			END IF
			V(IQ) = TOP
		END DO

	ENDDO

    DO I = 1,N
		V(I) = A(I,I)
	ENDDO

	IF (NORD==1) RETURN
	DO I = 1,N
		DO J = I+1,N
			IF (V(J)<V(I)) THEN
				VV=V(J)
				V(J)=V(I)
				V(I)=VV
				DO K = 1,N
					VV=X(K,J)
					X(K,J)=X(K,I)
					X(K,I)=VV
				ENDDO
			ENDIF
		ENDDO
	ENDDO
    RETURN
    END

    
! --------------------------------------------------------------
!   SUBROUTINE TO SOLVE EQUATIONS SYSTEM,
!   GAUSS-JORDAN MATRIX INVERSION AND LINEAR EQUATION SOLUTION
!   NUMERICAL RECIPES PG. 30 (MODIFIED)
! --------------------------------------------------------------
!	  a      : SYSTEM MATRIX (SQUARE)
!	  n      : SYSTEM ORDER = "a" DIMENSION
!     np     : MAX DIMENSION OF THE COEFICIENTS MATRIX
!     b      : MATRIX OF INDEPENDIENT TERMS. AFTER EXECUTING THE FUNCTION THEY TURN INTO THE FIT COEFFICIENT 
!     m      : DIMENSION OF THE MATRIX OF INDEPENDIENT TERMS
!     mp     : MAX DIMENSION OF THE MATRIX OF INDEPENDIENT TERMS
! --------------------------------------------------------------------

SUBROUTINE DGAUSSJ(a,n,np,b,m,mp)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INTEGER m,mp,n,np,NptoMax
      REAL*8 a(np,np),b(np,mp)
      PARAMETER (NptoMax=100)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NptoMax),indxr(NptoMax),ipiv(NptoMax)
      REAL*8 big,dum,pivinv
	  
	  do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                STOP 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) THEN
			STOP 'singular matrix in gaussj'
		endif
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
     return
    END
      
SUBROUTINE QGAUSSJ(a,n,np,b,m,mp)
      IMPLICIT REAL*16 (A-H,O-Z)
      INTEGER m,mp,n,np,NptoMax
      REAL*16 a(np,np),b(np,mp)
      PARAMETER (NptoMax=100)
      INTEGER i,icol,irow,j,k,l,ll,indxc(NptoMax),indxr(NptoMax),ipiv(NptoMax)
      REAL*16 big,dum,pivinv
	  
	  do 11 j=1,n
        ipiv(j)=0
11    continue
      do 22 i=1,n
        big=0.
        do 13 j=1,n
          if(ipiv(j).ne.1)then
            do 12 k=1,n
              if (ipiv(k).eq.0) then
                if (abs(a(j,k)).ge.big)then
                  big=abs(a(j,k))
                  irow=j
                  icol=k
                endif
              else if (ipiv(k).gt.1) then
                STOP 'singular matrix in gaussj'
              endif
12          continue
          endif
13      continue
        ipiv(icol)=ipiv(icol)+1
        if (irow.ne.icol) then
          do 14 l=1,n
            dum=a(irow,l)
            a(irow,l)=a(icol,l)
            a(icol,l)=dum
14        continue
          do 15 l=1,m
            dum=b(irow,l)
            b(irow,l)=b(icol,l)
            b(icol,l)=dum
15        continue
        endif
        indxr(i)=irow
        indxc(i)=icol
        if (a(icol,icol).eq.0.) THEN
			STOP 'singular matrix in gaussj'
		endif
        pivinv=1./a(icol,icol)
        a(icol,icol)=1.
        do 16 l=1,n
          a(icol,l)=a(icol,l)*pivinv
16      continue
        do 17 l=1,m
          b(icol,l)=b(icol,l)*pivinv
17      continue
        do 21 ll=1,n
          if(ll.ne.icol)then
            dum=a(ll,icol)
            a(ll,icol)=0.
            do 18 l=1,n
              a(ll,l)=a(ll,l)-a(icol,l)*dum
18          continue
            do 19 l=1,m
              b(ll,l)=b(ll,l)-b(icol,l)*dum
19          continue
          endif
21      continue
22    continue
      do 24 l=n,1,-1
        if(indxr(l).ne.indxc(l))then
          do 23 k=1,n
            dum=a(k,indxr(l))
            a(k,indxr(l))=a(k,indxc(l))
            a(k,indxc(l))=dum
23        continue
        endif
24    continue
     return
    END

 
    
!!====================================================================================
!!              SUBROUTINES RELATED TO COORDINATES CONVERSION
!!====================================================================================

! ------------------------------------------------------------
!   SUBROUTINE TO CONVERT CARTESIAN COORDINATES TO POLAR
!   RECEIVES THE DATA X,Y,Z AND RETURNS R, COS(THETA) AND PHI
! ------------------------------------------------------------
    
SUBROUTINE DCAR2SPHER(P,PP)
	IMPLICIT NONE
	REAL*8 P(1:3),PP(1:3)
	REAL*8 PI

	PI=4.D0*DATAN(1.D0)

	PP(1)=DSQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
	IF (PP(1)==0.D0)THEN
		PP(2)=0.D0
		PP(3)=0.D0
	ELSE
		PP(2)=P(3)/PP(1)
	END IF
	PP(3)=DSIGN(1.D0,P(2))*PI/2.D0
    IF(P(1)*P(1)>0.D0)PP(3)=DATAN2(P(2),P(1))

END 

SUBROUTINE QCAR2SPHER(P,PP)
	IMPLICIT NONE
	REAL*16 P(1:3),PP(1:3)
	REAL*16 PI

	PI=4.Q0*QATAN(1.Q0)

	PP(1)=QSQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
	IF (PP(1)==0.Q0)THEN
		PP(2)=0.Q0
		PP(3)=0.Q0
	ELSE
		PP(2)=P(3)/PP(1)
	END IF
	PP(3)=QSIGN(1.Q0,P(2))*PI/2.Q0
    IF(P(1)*P(1)>0.Q0)PP(3)=QATAN2(P(2),P(1))

    END 

! --------------------------------------------------------------------
!   SUBROUTINE TO CONVERT POLAR COORDINATES TO CARTESIAN
!   RECEIVES A VECTOR R, COS(THETA), PHI AND RETURNS OTHER WITH X,Y,Z
! --------------------------------------------------------------------
    
SUBROUTINE DSPHER2CAR(PP,P)
	IMPLICIT NONE
	REAL*8 P(1:3),PP(1:3)
	REAL*8 AUX

	AUX=PP(1)*DSQRT(1.D0-PP(2)**2)
	P(1)=AUX*DCOS(PP(3))
	P(2)=AUX*DSIN(PP(3))
	P(3)=PP(1)*PP(2)
    END 
    
SUBROUTINE QSPHER2CAR(PP,P)
	IMPLICIT NONE
	REAL*16 P(1:3),PP(1:3)
	REAL*16 AUX

	AUX=PP(1)*QSQRT(1.Q0-PP(2)**2)
	P(1)=AUX*QCOS(PP(3))
	P(2)=AUX*QSIN(PP(3))
	P(3)=PP(1)*PP(2)
    END 

! ----------------------------------------------------------------------------------
!   SUBROUTINE TO CONVERT ELLIPSOIDAL COORDINATES TO CARTESIAN
!   RECEIVES A VECTOR R=LAMBDA, COS(THETA)=MU, PHI=PHI AND RETURNS OTHER WITH X,Y,Z
! ----------------------------------------------------------------------------------
        
SUBROUTINE DELLIP2CAR(PP,DISTCENTROS,P)
	IMPLICIT NONE
	REAL*8 P(1:3),PP(1:3)
	REAL*8 AUX,DISTCENTROS

	AUX=DISTCENTROS*DSQRT((PP(1)**2-1.D0)*(1.D0-PP(2)**2))/2.D0
	P(1)=AUX*DCOS(PP(3))
	P(2)=AUX*DSIN(PP(3))
	P(3)=DISTCENTROS*PP(1)*PP(2)/2.D0
END
   
SUBROUTINE QELLIP2CAR(PP,CENTERSDIST,P)
	IMPLICIT NONE
	REAL*16 P(1:3),PP(1:3)
	REAL*16 AUX,CENTERSDIST

	AUX=CENTERSDIST*QSQRT((PP(1)**2-1.Q0)*(1.Q0-PP(2)**2))/2.Q0
	P(1)=AUX*QCOS(PP(3))
	P(2)=AUX*QSIN(PP(3))
	P(3)=CENTERSDIST*PP(1)*PP(2)/2.Q0
    END



!!====================================================================================
!!              SUBROUTINES AND FUCNTIONS TO DETERMINE THE TIME
!!====================================================================================

! -----------------------------------
!   SUBROUTINE TO GET THE TIME TAKEN
! -----------------------------------

SUBROUTINE CAL_TIME(SS,D,H,M,S,T)
	REAL*8 SS,S,AUX,AUX1
	INTEGER*4 D,H,M
	CHARACTER*(*) T
    CHARACTER*100 INTTOSTR,REALTOSTR,SAUX
	
	AUX=INT(SS/60.D0)
	S=MOD(SS,60.D0)
	AUX1=INT(AUX/60.D0)
	M=MOD(AUX,60.D0)
	D=INT(AUX1/24.D0)
	H=MOD(AUX1,24.D0)

	T=''
	IF (D/=0)T=TRIM(INTTOSTR(D))//' D  '
	IF (H/=0)T=TRIM(T)//TRIM(INTTOSTR(H))//' H  '
	IF (M/=0)T=TRIM(T)//TRIM(INTTOSTR(M))//" '  "
    WRITE(SAUX,'(F5.2)')S
    T=TRIM(T)//TRIM(SAUX)//" ''"

	RETURN
    END

    
!=======================================================================================
!   SUBROUTINES OF NUMBERS AND DATA CONVERTION (NUMBERS TO STRINGS AND STRINGS TO NUMBERS)
!=======================================================================================

!--------------------------------------------------------
!   SUBROUTINES THAT RETURNS A REAL NUMBER FROM A STRING
!--------------------------------------------------------

REAL*8 FUNCTION STRTODBLE(STR)

CHARACTER(LEN=*) STR
CHARACTER(LEN=LEN(STR)),AUTOMATIC :: STR1
LOGICAL SIGNO,ISNUMBER
INTEGER*2 I,K,LU,IIN,IFR


	STR1=STR

	SIGNO=.FALSE.
	STRTODBLE=0.D0
    LU=LEN_TRIM(STR1)
	
! CHECK IF IT IS A NUMBER
	IF(.NOT.ISNUMBER(STR1))THEN
		RETURN
	ENDIF
  
! CHECK THE SIGN
    IF(STR1(1:1)=='-')then
		SIGNO=.TRUE.
		STR1=STR1(2:LU)
		LU=LU-1
    ENDIF

! CHECK IF THE NUMBER IS REAL OR INTEGER
    IF(INDEX(STR1,'.')/=0)THEN
		IIN=INDEX(STR1,'.')-1
	ELSE
		IIN=LU
	ENDIF

	IFR=LU-(IIN+1)

! GET THE INTEGER PART OF THE NUMBER
	DO I=1,IIN
		K=ICHAR(STR1(I:I))-48
		STRTODBLE=STRTODBLE+DBLE(K)*10.D0**DBLE(IIN-I)
    ENDDO

	IF(IIN==LU)THEN
		IF(SIGNO)STRTODBLE=-STRTODBLE
	ELSE
		STR1=STR1(IIN+2:LU)
! GET THE DECIMAL PART OF THE NUMBER
		DO I=1,IFR
			K=ICHAR(STR1(I:I))-48
			STRTODBLE=STRTODBLE+DBLE(K)/10.D0**DBLE(I)
		ENDDO

	    IF(SIGNO)STRTODBLE=-STRTODBLE
	ENDIF
	RETURN
    END
    
REAL*16 FUNCTION STRTOQUAD(STR)
    CHARACTER(LEN=*) STR
    CHARACTER(LEN=LEN(STR)),AUTOMATIC :: STR1
    LOGICAL SIGNO,ISNUMBER
    INTEGER*2 I,K,LU,IIN,IFR

	STR1=STR

	SIGNO=.FALSE.
	STRTOQUAD=0.Q0
    LU=LEN_TRIM(STR1)
	
! CHECK IF IT IS A NUMBER
	IF(.NOT. ISNUMBER(STR1))THEN
		RETURN
	ENDIF
  
! CHECK THE SIGN
    IF(STR1(1:1)=='-')then
		SIGNO=.TRUE.
		STR1=STR1(2:LU)
		LU=LU-1
    ENDIF

! CHECK IF THE NUMBER IS REAL OR INTEGER
    IF(INDEX(STR1,'.')/=0)THEN
		IIN=INDEX(STR1,'.')-1
	ELSE
		IIN=LU
	ENDIF

	IFR=LU-(IIN+1)

! GET THE INTEGER PART OF THE NUMBER
	DO I=1,IIN
		K=ICHAR(STR1(I:I))-48
		QSTRTOQUAD=QSTRTOQUAD+REAL(K,16)*10.Q0**REAL(IIN-I,16)
    ENDDO

	IF(IIN==LU)THEN
		IF(SIGNO)QSTRTOQUAD=-QSTRTOQUAD
	ELSE
		STR1=STR1(IIN+2:LU)
        
! GET THE DECIMAL PART OF THE NUMBER
		DO I=1,IFR
			K=ICHAR(STR1(I:I))-48
			STRTOQUAD=STRTOQUAD+REAL(K,16)/10.Q0**REAL(I,16)
		ENDDO

	    IF(SIGNO)STRTOQUAD=-STRTOQUAD
	ENDIF
	RETURN
    END

! --------------------------------------------------------------
!     FUNCTION THAT RECEIVES A REAL NUMBER AND RETURNS A STRING
! --------------------------------------------------------------
CHARACTER*(*) FUNCTION REALDTOSTR(NUMBER)
	REAL*8 NUMBER
        CHARACTER*100 CVALUE

	REALDTOSTR=""
	WRITE(CVALUE,*)NUMBER
	REALDTOSTR=TRIM(ADJUSTL(CVALUE))
    END
    
CHARACTER*(*) FUNCTION REALQTOSTR(NUMERO)
	REAL*16 NUMERO
        CHARACTER*100 CVALOR

	REALQTOSTR=""
	WRITE(CVALOR,*)NUMERO
	REALQTOSTR=TRIM(ADJUSTL(CVALOR))
    END
    

!----------------------------------------------------
!   SUBROUTINE THAT RETURNS AN INTEGER FROM A STRING
!----------------------------------------------------

INTEGER*2 FUNCTION STRTOINT(STR)

    CHARACTER(LEN=*) STR
    CHARACTER(LEN=LEN(STR)),AUTOMATIC :: STR1
    LOGICAL SIGNO, ISNUMBER
    INTEGER*2 I,K,LU

	STR1=ADJUSTL(STR)

	SIGNO=.FALSE.
	STRTOINT=0
    LU=LEN_TRIM(STR1)

! CHECK IF A NUMBER
	IF(.NOT. ISNUMBER(TRIM(STR1)))THEN
		RETURN
	ENDIF

! CHECK THE SIGN
    IF(STR1(1:1)=='-')then
		SIGNO=.TRUE.
		STR1=STR1(2:LU)
		LU=LU-1
    ENDIF

! GET THE INTEGER PART OF THE NUMBER
	DO I=1,LU
		K=ICHAR(STR1(I:I))-48
		STRTOINT=STRTOINT+K*10**(LU-I)
    ENDDO

	IF(SIGNO)STRTOINT=-STRTOINT
	RETURN
END

! --------------------------------------------------------------------------
!     FUNCTION THAT RECEIVES A POSITIVE INTEGER NUMBER AND RETURNS A STRING.
! --------------------------------------------------------------------------

CHARACTER*(*) FUNCTION INTTOSTR(NUMBER)
	INTEGER*2 NUMBER
	CHARACTER*100 CVALUE

	INTTOSTR=""
	WRITE(CVALUE,*)NUMBER
	INTTOSTR=ADJUSTL(CVALUE)
END

!------------------------------------------------------------------
!   FUNCTION THAT RETURNS TRUE IN CASE THE STRING GIVEN IS NUMERIC.
!------------------------------------------------------------------

LOGICAL FUNCTION ISNUMBER(STR)
  IMPLICIT NONE
  CHARACTER(len=*), INTENT(IN) :: STR
  REAL :: X
  INTEGER :: E
  READ(STR,*,IOSTAT=E) X
  ISNUMBER = E == 0
END

! ------------------------------------------------------
!     FUNCION THAT INDICATES IF A NUMBER IS ODD OR EVEN.
! ------------------------------------------------------

LOGICAL FUNCTION ISPAR(N)
	INTEGER*4 N

	ISPAR=.TRUE.
	IF(MOD(N,2) /= 0)ISPAR=.FALSE.
	RETURN
END

! ------------------------------------------------------------------------------
!    FUNCTION THAT RETURNS TRUE IF THE NUMBER IS AN INTEGER AND FALSE OTHERWISE.
! ------------------------------------------------------------------------------

LOGICAL FUNCTION ISINT(NUMI)
	REAL*16 NUMI,NUM
    
	NUM=QABS(NUMI)
	IF(NUM-QINT(NUM)>0.Q0)THEN
		ISINT=.FALSE.
	ELSE
		ISINT=.TRUE.
	ENDIF
    END

    
!=======================================================================================
!   SUBROUTINES AND FUNCTIONS FOR TREATING STRINGS.
!=======================================================================================

!----------------------------------------------------------------------------------------
!   SUBROUTINE TO CONVERT A STRING INTO A ARRAY OF WORDS DEPENDING ON THE WORD SEPARATOR.
!----------------------------------------------------------------------------------------

SUBROUTINE SPLIT(STRING,SPLITTER,SUBSTRING)
	CHARACTER*(*) STRING
	CHARACTER*1 SPLITTER,CAD
	INTEGER*2 L,I,NSUB,FLAG,INI(1:50),FIN(1:50)
	CHARACTER*(*) SUBSTRING(1:50)
	CHARACTER*300 STRING1
	
	STRING1=ADJUSTL(STRING)  !REMOVE THE BLANK SPACE OF THE BEGINNING AND PLACE IT AT THE END

	FLAG=0 !TELLS IF A BLANK CHARACTER IS READ
	NSUB=1
	INI(1)=1
	L=LEN_TRIM(STRING1)
	DO I=1,L
		CAD=STRING1(I:I)
		IF (CAD==SPLITTER) THEN
			IF(FLAG==0)THEN
				FLAG=1
				FIN(NSUB)=I-1
				NSUB=NSUB+1
				INI(NSUB)=I+1
			ELSE
				INI(NSUB)=INI(NSUB)+1
			ENDIF
		ELSE
			FLAG=0
		ENDIF
	ENDDO
	FIN(NSUB)=L
	
	DO I=1,NSUB
		SUBSTRING(I)=STRING1(INI(I):FIN(I))
	ENDDO
	DO I=1,NSUB
		SUBSTRING(I)=ADJUSTL(SUBSTRING(I))
	ENDDO
    END SUBROUTINE

    
    
    !-----------------------------------------------------------------------------
!   SUBROUTINE TO CONVERT ALL THE CHARACTER TO CAPITAL CHARACTER IF (MM=1) OR  
!   TO SMALL LETTER IF (MM=0).
!-----------------------------------------------------------------------------

SUBROUTINE CMAYMIN(C,MM)
	CHARACTER*(*) C
	INTEGER*2 MM,I
	
	IF (MM==1) THEN
		DO I=1,LEN(C)
			IF (ICHAR(C(I:I))>=97.AND.ICHAR(C(I:I))<=122)THEN
				C(I:I)=CHAR(ICHAR(C(I:I))-32)
			ELSEIF (ICHAR(C(I:I))==0)THEN
				C(I:I)=CHAR(32)
			ENDIF
		ENDDO
	ELSE IF (MM==0) THEN
		DO I=1,LEN(C)
			IF (ICHAR(C(I:I))>=65.AND.ICHAR(C(I:I))<=90)THEN
				C(I:I)=CHAR(ICHAR(C(I:I))+32)
			ELSEIF (ICHAR(C(I:I))==0)THEN
				C(I:I)=CHAR(32)
			ENDIF
		ENDDO
	ENDIF
    END

     
    
!----------------------------------------------------------------------------------
!   FUNCTION TO COMPARE TWO STRINGS. RETURNS 1 IF THEY ARE EQUALS AND 0 OHTERWISE. 
!   IT IS NOT CASE SENSITIVE.
!----------------------------------------------------------------------------------

LOGICAL FUNCTION COMPARESTR(C1,C2)
	CHARACTER*(*), INTENT(IN) ::  C1,C2
	INTEGER*2 I,L
			
	CALL CMAYMIN(C1,1)
	CALL CMAYMIN(C2,1)
	
	IF (LEN_TRIM(C1).NE.LEN_TRIM(C2)) THEN
		COMPARESTR=.FALSE.
		RETURN
	ELSE
		L=LEN_TRIM(C1)
	ENDIF
	
	DO I=1,L
		IF(C1(I:I).NE.C2(I:I))THEN
			COMPARESTR=.FALSE.
			RETURN
		ENDIF
	ENDDO
	COMPARESTR=.TRUE.
END


    
!======================================================================
!   SUBROUTINES TO IMPROVE THE DATA WRITING     
!======================================================================

!----------------------------------------      
!   SUBROUTINE FOR ADDING N BLANK LINES.                     
!----------------------------------------

SUBROUTINE BLANKLINES(N,NUNIT)
	INTEGER*4 N,NUNIT

	IF(NUNIT.EQ.0)THEN
		DO I=1,N
			WRITE(*,*)'  '
		ENDDO
	ELSE
		DO I=1,N
			WRITE(NUNIT,*)'  '
		ENDDO
	ENDIF
	RETURN
    END

    
!------------------------------------------
!   SUBROUTINE TO ADD A LINE WITH N DASHES.
!------------------------------------------

SUBROUTINE CHARDASH(N,NUNIT,J)
	INTEGER*4 N,J,NUNIT
	DIMENSION M(80)
	CHARACTER M*1
	DO I=1,80
		IF(J.EQ.1)M(I)='-'
		IF(J.EQ.2)M(I)='='
		IF(J.EQ.3)M(I)='*' 
		IF(J.EQ.4)M(I)='+'    
		IF(J.EQ.5)M(I)='.'
	ENDDO
	IF(J.EQ.6)THEN
		DO I=1,N/2
			M(I)='<'
		ENDDO
		DO I=N/2+1,N
			M(I)='>'
		ENDDO
	ENDIF
	IF(NUNIT.NE.0)THEN
		WRITE(NUNIT,*)(M(I),I=1,N)
	ENDIF
	RETURN
    END

 
    
!----------------------------     
!   SUBROUTINE TO SPLIT DATA.
!----------------------------

SUBROUTINE SPLITTER(NCHAR,CHAR,NLINES,SFILE)
	INTEGER*4 NLINES,NCHAR,CHAR,SFILE
	
	CALL BLANKLINES(NLINES,SFILE)
	CALL CHARDASH(NCHAR,SFILE,CHAR)
	CALL BLANKLINES(NLINES,SFILE)
END


!=======================================================================================
! SUBROUTINES FOR THE TREATMENT OF FILES 
!=======================================================================================

! ------------------------------------------------------
! FUNCTION THAT RETURNS THE NUMBER OF THE NON-USED FILE.
! ------------------------------------------------------

INTEGER(KIND=4) FUNCTION GET_UNIT()
    IMPLICIT NONE
    
    INTEGER (KIND=4) I
    INTEGER (KIND=4) IOS
    LOGICAL LOPEN
    
    GET_UNIT=0
    DO I=1,99
        IF(I/=5.AND.I/=6.AND.I/=9)THEN
            INQUIRE(UNIT=I,OPENED=LOPEN,IOSTAT=IOS)
            IF(IOS==0)THEN
                GET_UNIT=I
                RETURN
            ENDIF
        ENDIF
    ENDDO
    RETURN
END FUNCTION

! -------------------------------
! FUNCTION THAT RETURNS THE PATH.
! -------------------------------

CHARACTER(LEN=*) FUNCTION GET_PATH(STR)
    USE CONST
    IMPLICIT NONE
    
    CHARACTER(LEN=LONG)::STR
    INTEGER(KIND=4):: POSCHAR,POS
    
    POS=POSCHAR(STR,'/',.TRUE.) !THIS BAR IS USED FOR THE SEPARATION OF FOLDERS IN WINDOWS AND LINUX
    GET_PATH=STR(1:POS)
    RETURN
END FUNCTION

! -------------------------------------------------------------------
! FUNCTION THAT RETURNS THE COMPLETE NAME OF THE FILE (W/ EXTENSION).
! -------------------------------------------------------------------

CHARACTER(LEN=*) FUNCTION GET_FILENAME(STR)
    IMPLICIT NONE
    
    CHARACTER(LEN=*)::STR
    INTEGER(KIND=4):: POSCHAR,POS

    POS=POSCHAR(STR,'/',.TRUE.) !THIS BAR IS USED FOR THE SEPARATION OF FOLDERS IN WINDOWS AND LINUX
    IF (POS<=0)THEN
        GET_FILENAME=STR
    ELSE
        GET_FILENAME=STR(POS+1:)
    ENDIF
    RETURN
END FUNCTION    

! ---------------------------------------------------------------------
! FUNCTION THAT RETURNS THE COMPLETE NAME OF THE FILE (WO/ EXTENSION).
! ---------------------------------------------------------------------

CHARACTER(LEN=*) FUNCTION GET_BASENAME(STR)
    IMPLICIT NONE
    
    CHARACTER(LEN=*)::STR
    INTEGER(KIND=4):: POSCHAR,POS,POS1

    POS=POSCHAR(STR,'/',.TRUE.) !THIS BAR IS USED FOR THE SEPARATION OF FOLDERS IN WINDOWS AND LINUX   
    POS1=POSCHAR(STR,'.',.TRUE.)
    IF (POS<=0)THEN
        GET_BASENAME=STR(1:POS1-1)
    ELSE
        
        GET_BASENAME=STR(POS+1:POS1-1)
    ENDIF
    RETURN
END FUNCTION    
    
! ------------------------------------------
! FUNCTION THAT RETURNS THE FILE EXTENSION.
! ------------------------------------------

CHARACTER(LEN=*) FUNCTION GET_EXT(STR)
    IMPLICIT NONE
    
    CHARACTER(LEN=*)::STR
    INTEGER(KIND=4):: POSCHAR,POS

    POS=POSCHAR(STR,'.',.TRUE.)
    GET_EXT=STR(POS+1:)
    RETURN
END FUNCTION 

! ------------------------------------------------------------------------------------------------
! FUNCTION THAT RETURNS THE FILE CHANGING THE EXTENSION (IFPATH=TRUE/FALSE WITH PATH/WITHOUTPATH).
! ------------------------------------------------------------------------------------------------

CHARACTER(LEN=*) FUNCTION REPLACE_EXT(STR,NEWEXT,IFPATH)
    IMPLICIT NONE
    
    CHARACTER(LEN=*)::STR,NEWEXT
    CHARACTER(LEN=150)::AUX,GET_FILENAME
    INTEGER(KIND=4)::POSCHAR,POS
    LOGICAL(KIND=4)::IFPATH

    POS=POSCHAR(STR,'.',.TRUE.)
    AUX=STR(1:POS)//TRIM(NEWEXT)
    IF (IFPATH)THEN
        REPLACE_EXT=TRIM(AUX)
    ELSE
        REPLACE_EXT=GET_FILENAME(AUX)
    ENDIF
    RETURN
END FUNCTION   
    
! -----------------------------------------------------------------------------
! FUNCTION THAT RETURNS THE FILE CHANGING A CHARACTER (CHIN) FOR OTHER (CHOUT).
! -----------------------------------------------------------------------------    

FUNCTION REPLACE_CHAR(STR,CHIN,CHOUT)
    USE CONST
    IMPLICIT NONE
    
    CHARACTER(LEN=LONG)::STR, REPLACE_CHAR
    !CHARACTER(LEN=LEN(STR))::REPLACE_CHAR
    CHARACTER(LEN=1)::CHIN,CHOUT
    INTEGER(KIND=4)::I
    
    REPLACE_CHAR=STR
    DO I=1,LEN_TRIM(STR)
        IF (STR(I:I)==CHIN)THEN
            REPLACE_CHAR(I:I)=CHOUT
        !ELSE
        !    REPLACE_CHAR(I:I)=STR(I:I)
        ENDIF
    ENDDO
    RETURN
    END FUNCTION   

    
SUBROUTINE REPLACE_CHAR1(STRIN,CHIN,CHOUT,STROUT)
    USE CONST
    IMPLICIT NONE
    
    CHARACTER(LEN=*)::STRIN, STROUT
    CHARACTER(LEN=LEN(STRIN))::REPLACE_CHAR
    CHARACTER(LEN=1)::CHIN,CHOUT
    INTEGER(KIND=4)::I
    

    STROUT=STRIN
    DO I=1,LEN_TRIM(STRIN)
        IF (STRIN(I:I)==CHIN)THEN
            STROUT(I:I)=CHOUT
        !ELSE
        !    REPLACE_CHAR(I:I)=STR(I:I)
        ENDIF
    ENDDO

    STROUT=TRIM(STROUT)
    
    END SUBROUTINE   

    
    
! ------------------------------------------------------------------------------------------------------------
! FUNCTION THAT RETURNS THE POSITION OF THE FIRST/LAST (DIR=FALSE/TRUE) GIVEN CHARACTER (CH) IN THE STRING (STR)
! DISTINGUISH BETWEEN MAYUS AND MINUS.
! ------------------------------------------------------------------------------------------------------------

 INTEGER(KIND=4) FUNCTION POSCHAR(STR,CH,DIR)
    IMPLICIT NONE
    CHARACTER(LEN=*)::STR
    CHARACTER(LEN=1)::CH
    LOGICAL(KIND=2)::DIR
    INTEGER(KIND=4)::I,LENGTH
  
    POSCHAR=-1
    LENGTH=LEN_TRIM(STR)
    IF (DIR)THEN
        DO I=LENGTH,1,-1
            IF(STR(I:I)==CH) THEN
                POSCHAR=I
                RETURN
            ENDIF
        ENDDO
    ELSE
        DO I=1,LENGTH
            IF(STR(I:I)==CH) THEN
                POSCHAR=I
                RETURN
            ENDIF
        ENDDO
    ENDIF
    RETURN
    END FUNCTION
    

!!=======================================================================================    
!!              SUBROUTINES AND FUNCTIONS FOR INI FILES   
!!=======================================================================================   
    
    
    
! -----------------------------------------------------------------------------------
!   FUNCTION TO RETURN THE PATH OF THE .EXE SOFTWARE
! -------------------------------------------------------------------------------------
CHARACTER(LEN=*) FUNCTION EXE_PATH()
    USE CONST    
    IMPLICIT NONE
    
    CHARACTER(LEN=LONG):: ME, GET_PATH, REPLACE_CHAR
    
    CALL GET_COMMAND_ARGUMENT(0,ME) 
    ME=REPLACE_CHAR(ME,'\','/')
    EXE_PATH=GET_PATH(ME)
    RETURN
END
    
    
 ! -------------------------------------------------------------------
!    SUBROUTINE TO CREATE A NEW SECTION IN THE GIVEN FILE.
!    IF THE SECTION ALREADY EXISTS, THE SECTION IS NOT CREATED.
! -------------------------------------------------------------------
SUBROUTINE MAKESECTION(FILEN,SECTION)
	IMPLICIT NONE
	CHARACTER*(*) FILEN,SECTION
	INTEGER*4 I
	LOGICAL FOUND,COMPARESTR
	CHARACTER*100 LINE,LINEUX,SECTIONAUX
		
	LINE=""
	LINEUX=""
	SECTIONAUX=""
	FOUND=.FALSE.
	OPEN(1,FILE=FILEN,STATUS='UNKNOWN')
	DO WHILE (.NOT. EOF(1))
		READ(1,'(A100)')LINE
		IF (LINE(1:1)=='[') THEN
			!TO GET THE SECTION NAME WITHOUT CLASP
			DO I=2,LEN_TRIM(LINE)-1
				LINEUX(I-1:I-1)=LINE(I:I)
			ENDDO
			LINE=""
			LINE=LINEUX
			LINEUX=""
			SECTIONAUX=SECTION
			!IF THE SECTION IS FOUND THEN...
			IF (COMPARESTR(LINE,SECTIONAUX)==.TRUE.) THEN
				FOUND=.TRUE.
				IF (LEN_TRIM(LINE).NE.LEN_TRIM(SECTION))THEN
					FOUND=.FALSE.
				ELSE
					EXIT
				ENDIF
			ENDIF
		ENDIF
	ENDDO
	IF (FOUND==.FALSE.) THEN
		WRITE(1,'((/,A1,A,A1))')'[',TRIM(SECTION),']'
	ENDIF
	CLOSE(1)
END

! -------------------------------------------------------------------
!    SUBROUTINE TO SAVE THE VALUE ASSOCIATED TO THE KEY AND SECTION 
!    IF THE KEY DOES NOT EXIT, IT WILL BE CREATED, IF DOES, IT WILL BE AMENDED
! ---------------------------------------------------------------------
SUBROUTINE SAVEVALUE(FILEN,SECTION,KEY,VALUE_K)
	IMPLICIT NONE
	CHARACTER*(*) FILEN,SECTION,KEY,VALUE_K
	CHARACTER*100 LINE,LINE1,LINEUX,SECTIONAUX,KEYAUX
	LOGICAL COMPARESTR
	INTEGER*4 WRITTEN,I,POS  
		
	LINE=""
	LINE1=""
	LINEUX=""
	SECTIONAUX=""
	KEYAUX=""
	SECTIONAUX=SECTION
	CALL MAKESECTION(FILEN,SECTIONAUX)
	OPEN(1,FILE=FILEN,STATUS='OLD')
	OPEN(2,STATUS='SCRATCH')
		
	WRITTEN=0
	DO WHILE (.NOT. EOF(1))
		READ(1,'(A100)')LINE
		WRITE(2,'(A100)')LINE
		!CHECK IF A SECTION
		IF (LINE(1:1)=='[') THEN
			!GET THE NAME OF THE SECTION WITHOUT CLASPS
			DO I=2,LEN_TRIM(LINE)-1
				LINEUX(I-1:I-1)=LINE(I:I)
			ENDDO
			LINE=""
			LINE=LINEUX
			LINEUX=""
			!IF FOUND THE SECTION THEN...
			SECTIONAUX=""
			SECTIONAUX=SECTION
			IF (COMPARESTR(LINE,SECTIONAUX)==.TRUE.) THEN
				!SEARCH IN THE VALUE OF THE SECTION
				IF (EOF(1))THEN
					WRITE(1,*)
					WRITE(1,'((A\))')KEY,'=',TRIM(VALUE_K)
					CLOSE(1)
					CLOSE(2)
					WRITTEN=1
					RETURN
				ENDIF
				DO WHILE (.NOT. EOF(1))
					READ(1,'(A100)')LINE
					IF (LINE.NE."") WRITE(2,'(A100)')LINE
					!IF FOUND OTHER SECTION, THE VALUE WAS NOT FOUND
					IF (LINE(1:1)=='[') THEN
						BACKSPACE(2)
						WRITE(2,'((A\))')KEY,'=',TRIM(VALUE_K)
						WRITE(2,*)
						WRITE(2,*)
						BACKSPACE(1)
						WRITTEN=1
						EXIT
					ENDIF
						
					POS=INDEX(LINE,'=')
					LINEUX=""
					DO I=1,POS-1
						LINEUX(I:I)=LINE(I:I)
					ENDDO
					LINE1=LINEUX

					!IF THE LINE IS THE KEY
					KEYAUX=KEY
					IF(COMPARESTR(LINE1,KEYAUX)==.TRUE.)THEN
						BACKSPACE(2)
						WRITE(2,'((A\))')KEY,'=',TRIM(VALUE_K)
						WRITE(2,*)
						WRITTEN=1
						EXIT
					ENDIF
				ENDDO
				IF(WRITTEN==0)THEN
					WRITE(2,'((A\))')KEY,'=',TRIM(VALUE_K)
					WRITE(2,*)
				ENDIF
			ENDIF
		ENDIF
	ENDDO
	REWIND(1)
	REWIND(2)
	DO WHILE (.NOT. EOF(2))
		READ(2,'(A100)')LINE
		WRITE(1,'(A)')TRIM(LINE)
	ENDDO
	CLOSE(1)
	CLOSE(2)
    END

! -------------------------------------------------------------------      
!    FUNCTION TO READ A VALUE ASSOCIATED TO THE GIVEN KEY AND SECTION FROM THE DATA FILE.
!    AND CHECK THE COMPLIMENT OF THE ALLOWED OPTIONS. IF THE VALUE IS NOT FOUND, AN EMPTY 
!    STRING IS RETURNED.
! ------------------------------------------------------------------
CHARACTER*100 FUNCTION READVALUE_GEN(FILEN,SECTION,KEY,OPT_VALUES,NSUB)
    IMPLICIT NONE
	CHARACTER*(*) FILEN,SECTION,KEY
    CHARACTER*10 OPT_VALUES
    INTEGER*4 NSUB,I
    CHARACTER*1 SUBSTRING(1:50)
    CHARACTER*100 READVALUE
    LOGICAL FOUND_FLAG
    
    I=1
    FOUND_FLAG=.FALSE.
    
    CALL SPLIT(OPT_VALUES,',',SUBSTRING)

    READVALUE_GEN=READVALUE(FILEN,SECTION,KEY)
        
    DO WHILE(I<=NSUB)
        IF(TRIM(READVALUE_GEN).EQ.TRIM(SUBSTRING(I)))THEN
            FOUND_FLAG=.TRUE.
            EXIT
        ENDIF
        I=I+1
    ENDDO
    
    IF(FOUND_FLAG.EQ..FALSE.)THEN
    !    CALL ERR_MSG_STR_INPUT(FILEN,SECTION,KEY,SUBSTRING,NSUB) 
        WRITE(*,*)"INPUT DATA ERROR"
    ENDIF
END FUNCTION

! -------------------------------------------------------------------      
!    FUNCTION TO READ A VALUE ASSOCIATED TO THE GIVEN KEY AND SECTION FROM THE DATA FILE.
!    IF THE VALUE IS NOT FOUND, AN EMPTY STRING IS RETURNED.
! -------------------------------------------------------------------
CHARACTER*(*) FUNCTION READVALUE(FILEN,SECTION,KEY)
    IMPLICIT NONE
	CHARACTER*(*) FILEN,SECTION,KEY
	CHARACTER*100 LINE,LINE1,LINEUX,SECTIONAUX,KEYAUX
	INTEGER*4 I,POS
	LOGICAL COMPARESTR
		
	LINE=""
	LINE1=""
	LINEUX=""
	SECTIONAUX=""
	KEYAUX=""
    
    OPEN(10,FILE=FILEN,STATUS='OLD')
    DO WHILE (.NOT. EOF(10))
        READ(10,'(A100)')LINE
        !CHECK IF A SECTION
        IF (LINE(1:1)=='[') THEN
            !GET THE NAME OF THE SECTION WITHOUT CLASPS
            LINEUX=""
            DO I=2,LEN_TRIM(LINE)-1
                LINEUX(I-1:I-1)=LINE(I:I)
            ENDDO
            LINE=""
            LINE=LINEUX
            LINEUX=""
            !IF FOUND THE SECTION THEN...
            SECTIONAUX=SECTION
            IF (COMPARESTR(LINE,SECTIONAUX)==.TRUE.) THEN
                !SEARCH IN THE VALUE OF THE SECTION
                DO WHILE (.NOT. EOF(10))
                    READ(10,'(A100)')LINE
                    !IF FOUND OTHER SECTION, THE VALUE WAS NOT FOUND
                    IF (LINE(1:1)=='[') THEN
                        READVALUE=""
                        CLOSE(10)
                        RETURN
                    ENDIF
						
                    !BUSCA EL SIGNO =
                    POS=INDEX(LINE,'=')
                    LINEUX=""
                    DO I=1,POS-1
                        LINEUX(I:I)=LINE(I:I)
                    ENDDO
                    LINE1=LINEUX

                    !IF THE LINE IS THE KEY...
                    KEYAUX=KEY
                    IF(COMPARESTR(LINE1,KEYAUX)==.TRUE.)THEN
                        POS=INDEX(LINE,'=')
                        LINEUX=""
                        DO I=POS+1,LEN_TRIM(LINE)
                            LINEUX(I-POS:I-POS)=LINE(I:I)
                        ENDDO
                        READVALUE=LINEUX
                        CLOSE(10)
                        RETURN
                    ENDIF
                ENDDO
            ENDIF
        ENDIF
    ENDDO
    CLOSE(10)
END FUNCTION

! ----------------------------------------------------------------------------
!     FUNCTION TO READ AN INTEGER NUMBER FROM THE DATA FILE
! ----------------------------------------------------------------------------
INTEGER*4 FUNCTION READVALUE_INT(FILEN,SECTION,KEY,MINV,MAXV)
    IMPLICIT NONE
	CHARACTER*(*) FILEN,SECTION,KEY
	CHARACTER*100 CVALUE,READVALUE
    INTEGER*4 MINV,MAXV
    REAL*16 READVALUE_AUX
    LOGICAL ISNUMBER,ISINT
    
	CVALUE=READVALUE(FILEN,SECTION,KEY)
	IF (CVALUE=='') THEN
		READVALUE_INT=0
    ELSE
        IF(ISNUMBER(CVALUE))THEN
            READ(CVALUE,*)READVALUE_AUX
            IF(ISINT(READVALUE_AUX))THEN
                READ(CVALUE,*)READVALUE_INT
                IF(MAXV.LT.0)THEN
                    IF(READVALUE_INT.LT.MINV.OR.READVALUE_INT.GT.1000000)THEN
                        !CALL ERR_MSG_NUM_INPUT(FILEN,SECTION,KEY,3,MINV,MAXV)
                        WRITE(*,*)"INPUT DATA IS NOT INTO THE RANGE"
                    ENDIF
                ELSE
                    IF(READVALUE_INT.LT.MINV.OR.READVALUE_INT.GT.MAXV)THEN
                        !CALL ERR_MSG_NUM_INPUT(FILEN,SECTION,KEY,3,MINV,MAXV)
                        WRITE(*,*)"INPUT DATA IS NOT INTO THE RANGE"
                    ENDIF                    
                ENDIF
            ELSE
                !CALL ERR_MSG_NUM_INPUT(FILEN,SECTION,KEY,2,0,0)
                WRITE(*,*)"INPUT DATA IS NOT A VALID INTEGER NUMBER"
            ENDIF
        ELSE
		    !CALL ERR_MSG_NUM_INPUT(FILEN,SECTION,KEY,2,0,0)
            WRITE(*,*)"INPUT DATA IS NOT A VALID INTEGER NUMBER"
        ENDIF
    ENDIF
    END

! ----------------------------------------------------------------------------
!     FUNCTION TO READ A REAL NUMBER FROM THE DATA FILE
! ----------------------------------------------------------------------------
    
    REAL*16 FUNCTION READVALUE_REAL(FILEN,SECTION,KEY)
	IMPLICIT NONE
	CHARACTER*(*) FILEN,SECTION,KEY
	CHARACTER*100 CVALUE,READVALUE
    LOGICAL ISNUMBER
    
	CVALUE=READVALUE(FILEN,SECTION,KEY)
        
	IF (CVALUE=='') THEN
		    READVALUE_REAL=0.Q0
    ELSE
        IF(ISNUMBER(CVALUE))THEN
            READ(CVALUE,*)READVALUE_REAL
        ELSE
            !CALL ERR_MSG_NUM_INPUT(FILEN,SECTION,KEY,1,0,0)
            WRITE(*,*)"INPUT DATA IS NOT A VALID REAL NUMBER"
        ENDIF
    ENDIF
END

    
    
    
    
    
    
    
    
!=======================================================================================
! SUBROUTINES FOR THE TREATMENT OF NAMES AND SYMBOLS OF ATOMS
!=======================================================================================

! ------------------------------------------------------------------------------------
!  SUBROUTINE TO FIND OUT THE SYMBOL OF AN ELEMENT FROM ITS CHARGE (Z) AND VICE VERSA.
! ------------------------------------------------------------------------------------

SUBROUTINE CHARGETOSYMBOL(NUM,SB)
	INTEGER*2 I,NUM
	CHARACTER*2 SB
	CHARACTER*2 SYMBOL(103)
	LOGICAL COMPARESTR
	
	DATA (SYMBOL(I),I=1,103)/'H','He','Li','Be','B','C','N','O','F','Ne','Na','Mg', &
	'Al','Si','P','S','Cl','Ar','K','Ca','Sc','Ti','V','Cr','Mn','Fe','Co','Ni','Cu','Zn', &
	'Ga','Ge','As','Se','Br','Kr','Rb','Sr','Y','Zr','Nb','Mo','Tc','Ru','Rh','Pd','Ag', &
	'Cd','In','Sn','Sb','Te','I','Xe','Cs','Ba','La','Ce','Pr','Nd','Pm','Sm','Eu','Gd', &
	'Tb','Dy','Ho','Er','Tm','Yb','Lu','Hf','Ta','W','Re','Os','Ir','Pt','Au','Hg','Tl', &
	'Pb','Bi','Po','At','Rn','Fr','Ra','Ac','Th','Pa','U','Np','Pu','Am','Cm','Bk','Cf', &
	'Es','Fm','Md','No','Lr'/
	
	IF (NUM/=0)THEN
		IF(NUM>0.AND.NUM<104)THEN
			SB=SYMBOL(NUM)
		ELSE
			SB='0' !RETURNS THE CHARACTER 0 IF THE CHARGE IS NOT VALID
		ENDIF
	ELSE
		NUM=0 !RETURNS 0 IF IT IS NOT THE SYMBOL OF ANY ELEMENT
		DO I=1,103
			IF(COMPARESTR(SB,SYMBOL(I)))THEN
				NUM=I
				SB=SYMBOL(I)
				EXIT
			ENDIF
		ENDDO
	ENDIF
END

! -------------------------------------------------------------------------------------------
! SUBROUTINE TO FIND OUT THE NAME OF AN ELEMENT FROM ITS CHARGE (Z) AND VICE VERSA (SPANISH).
! -------------------------------------------------------------------------------------------

SUBROUTINE CHARGETONAMESP(NUM,NB)
	INTEGER*2 I,NUM
	CHARACTER*15 NB,CMAYMIN
	CHARACTER*15 NAME_SP(103)
	LOGICAL COMPARESTR
	
	DATA (NAME_SP(I),I=1,103)/'Hidrógeno','Helio','Litio','Berilio','Boro','Carbono', &
	'Nitrógeno','Oxígeno','Fluor','Neón','Sodio','Magnesio','Aluminio','Silicio','Fósforo', &
	'Azufre','Cloro','Argón','Potasio','Calcio','Escandio','Titanio','Vanadio','Cromo', &
	'Manganeso','Hierro','Cobalto','Níquel','Cobre','Zinc','Galio','Germanio','Arsénico', &
	'Selenio','Bromo','Kryptón','Rubidio','Estroncio','Itrio','Zirconio','Niobio','Molibdeno', &
	'Tecnecio','Rutenio','Rodio','Paladio','Plata','Cadmio','Indio','Estaño','Antimonio', &
	'Teluro','Iodo','Xenón','Cesio','Bario','Lantano','Cerio','Praseodimio','Neodimio', &
	'Promecio','Samario','Europio','Gadolinio','Terbio','Disprosio','Holmio','Erbio', &
	'Tulio','Iterbio','Lutecio','Hafnio','Tantalio','Wolframio','Renio','Osmio','Iridio', &
	'Platino','Oro','Mercurio','Talio','Plomo','Bismuto','Polonio','Ástato','Radón','Francio', &
	'Radio','Actinio','Torio','Protactinio','Uranio','Neptunio','Plutonio','Americio','Curio', &
	'Berkelio','Californio','Einstenio','Fermio','Mendelevio','Nobelio','Lawrencio'/

	IF (NUM/=0)THEN
		IF(NUM>0.AND.NUM<104)THEN
			NB=NAME_SP(NUM)
		ELSE
			NB='0' !RETURNS THE CHARACTER 0 IF THE CHARGE IS NOT VALID
		ENDIF
	ELSE
		NUM=0 !RETURNS 0 IF IT IS NOT THE SYMBOL OF ANY ELEMENT
		DO I=1,103
			IF(COMPARESTR(NB,NAME_SP(I)))THEN
				NUM=I
				NB=NAME_SP(I)
				EXIT
			ENDIF
		ENDDO
	ENDIF
END
		
! -------------------------------------------------------------------------------------------
! SUBROUTINE TO FIND OUT THE NAME OF AN ELEMENT FROM ITS CHARGE (Z) AND VICE VERSA (ENGLISH).
! -------------------------------------------------------------------------------------------

SUBROUTINE CHARGETONAMEEN(NUM,NB)
	INTEGER*2 I,NUM
	CHARACTER*15 NB,CMAYMIN
	CHARACTER*15 NAME_EN(103)
	LOGICAL COMPARESTR

	DATA (NAME_EN(I),I=1,103)/'Hydrogen','Helium','Lithium','Beryllium','Boron','Carbon', &
	'Nitrogen','Oxygen','Fluorine','Neon','Sodium','Magnesium','Aluminum','Silicon', &
	'Phosphorus','Sulfur','Chlorine','Argon','Potassium','Calcium','Scandium','Titanium', &
	'Vanadium','Chromium','Manganese','Iron','Cobalt','Nickel','Copper','Zinc','Gallium', &
	'Germanium','Arsenic','Selenium','Bromine','Krypton','Rubidium','Strontium','Yttrium', &
	'Zirconium','Niobium','Molybdenum','Technetium','Ruthenium','Rhodium','Palladium','Silver', &
	'Cadmium','Indium','Tin','Antimony','Tellurium','Iodine','Xenon','Cesium','Barium', &
	'Lanthanum','Cerium','Praseodymium','Neodymium','Promethium','Samarium','Europium', &
	'Gadolinium','Terbium','Dysprosium','Holmium','Erbium','Thulium','Ytterbium','Lutetium', &
	'Hafnium','Tantalum','Tungsten','Rhenium','Osmium','Iridium','Platinum','Gold','Mercury', &
	'Thallium','Lead','Bismuth','Polonium','Astatine','Radon','Francium','Radium','Actinium', &
	'Thorium','Protactinium','Uranium','Neptunium','Plutonium','Americium','Curium', &
	'Berkelium','Californium','Einsteinium','Fermium','Mendelevium','Nobelium','Lawrencium'/
	
	IF (NUM/=0)THEN
		IF(NUM>0.AND.NUM<104)THEN
			NB=NAME_EN(NUM)
		ELSE
			NB='0' !RETURNS THE CHARACTER 0 IF THE CHARGE IS NOT VALID
		ENDIF
	ELSE
		NUM=0 !RETURNS 0 IF IT IS NOT THE SYMBOL OF ANY ELEMENT
		DO I=1,103
			IF(COMPARESTR(NB,NAME_EN(I)))THEN
				NUM=I
				NB=NAME_EN(I)
				EXIT
			ENDIF
		ENDDO
	ENDIF
    END    