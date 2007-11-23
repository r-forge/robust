C
C Rocovm.f: FORTRAN subroutines for Rocke's covariances
C
      SUBROUTINE RLMCVROC(X,A,T,XC,XM,B0,NOBS,NVAR,NCOV,MDX,
     1                    TAU,MAXIT,NITMON,ILOC,ICNV,TOL,
     2                    NIT,XK,DIST,SA,ST,SR,SD)
C
C  FIXED POINT ALGORITHM FOR ROBUST COVARIANCES & S-CONDITION
C  Same args as Robeth subroutine CYFALG (without externals and common blocks)
C
      implicit double precision (a-h,o-z)
      DIMENSION X(MDX,NVAR),T(NVAR),DIST(NOBS)
      DIMENSION A(NCOV),SA(NCOV),SR(NVAR),ST(NCOV),
     +       SU(1),SUP(1),SD(NVAR)
      integer  rlicnvbi,rlicnhmc
      EXTERNAL RLICNVBI,RLICNHMC
      DATA TL/1.D-10/
C
C  STEP 0 : INITIALIZATION
C  ------
      IALG=1
      NU=1
      NIT=0
      HMAX=10.d0*TOL
      DO 10 I=1,NVAR
   10 SR(I)=HMAX
      IF (ICNV.EQ.1) THEN
         L=0
        DO 30 I=1,NVAR
        DO 20 J=1,I
        L=L+1
        SA(L)=0.D0
        IF (I.EQ.J) SA(L)=-1.D0
   20   CONTINUE
   30   CONTINUE
      ENDIF
      DO 40 L=1,NOBS
   40 DIST(L)=-1.d0
C
C  STEP 1: COMPUTE WEIGHTED COVARIANCE (ST) AND AUXILIARY VALUES
C  ------
  100 CALL RLUVROC(X,A,T,ST,NOBS,NVAR,NCOV,MDX,
     1     MDX,NU,IALG,ICNV,ILOC,TL,XC,XM,B0,XK,DELTA,DIST,
     2     SV,SV,SW,SR,SU,SUP,X,SD)
      IF (DABS(SV).LE.TL) CALL RLMESSGE(401,'RLMCVROC',0)
C
C  STEP 2: CHECK CONVERGENCE
C  ------
      IF (NIT.EQ.MAXIT) GOTO 600
      IF (RLICNVBI(NCOV,DELTA,A,SA,TOL,ICNV).EQ.1) THEN
         IF (ILOC.EQ.0) GOTO 600
         IF (RLICNHMC(NVAR,HMAX,SR,TOL,ICNV).EQ.1) GOTO 600
      ENDIF
C
C  STEP 3: FIND IMPROVEMENT MATRIX I-SS FOR A
C  ------
      INFO=0
      CALL RLPRSFBI(ST,NVAR,NCOV,TAU,INFO)
      IF (INFO.NE.0) CALL RLMESSGE(410+INFO,'RLMCVROC',0)
C
C  STEP 4: FIND AN IMPROVMENT VECTOR H FOR T
C  ------
      IF (ILOC.EQ.0) GOTO 500
      IF (DABS(SW).LE.TL) CALL RLMESSGE(402,'RLMCVROC',0)
      IF (DABS(SV).LE.TL.OR.DABS(SW).LE.TL) RETURN
      HMAX=0.d0
      DO 400 I=1,NVAR
        SR(I)=SR(I)/SW
        SRI=SR(I)
        HMAX=DMAX1(HMAX,DABS(SRI))
        T(I)=T(I)+SRI
  400 CONTINUE
C
C  STEP 5: SET SA:=A AND A:=(I-SS)*SA
C  ------
  500 DO 510 IJ=1,NCOV
  510 SA(IJ)=A(IJ)
      CALL RLMTT3BI(SA,ST,A,NVAR,NCOV)
      NIT=NIT+1
C
C  STEP 5A: ITERATION MONITORING
C  -------

      GOTO 100

C
C  STEP 6: EXIT
C  ------
  600 RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE RLUVROC(X,SA,T,ST,N,NP,NCOV,MDX,MDZ,NU,
     1           IALG,ICNV,ILOC,TL,XC,XM,B0,XK,DELTA,DIST,
     2           S1,S1P,S2,SR,SU,SUP,SZ,SD)
C
C  COMPUTE WEIGHTED COVARIANCE MATRIX AND CONSTANT K;
C  IF (IALG.NE.1) STORE EXU VALUES IN SU AND EXUP VALUES IN SUP;
C  RLUPROCV, RLVPROCV AND RLWPROCV ARE NOT USED IF IALG=1;
C  SZ IS NOT USED IF IALG.LE.2
C
      implicit double precision (a-h,o-z)
      DIMENSION X(MDX,NP),T(NP),DIST(N),SZ(MDZ,NP),V(4)
      DIMENSION SA(NCOV),ST(NCOV),SR(NP),SU(NU),SUP(NU),SD(NP)
      EXTERNAL  RLUROCV,RLUPROCV,RLVROCV,RLVPROCV,RLWROCV,RLWPROCV

      V(1)=XC
      V(2)=XM
      V(3)=DBLE(NP)
      V(4)=B0
      XN=DBLE(N)
      XP=DBLE(NP)
      TOL=1.D-4
      DELTA=0.d0
      S1=0.D0
      S1P=0.D0
      S2=0.D0
      DO 10 I=1,NP
   10 SR(I)=0.D0
      DO 20 IJ=1,NCOV
   20 ST(IJ)=0.D0
 
      DO 28 L=1,N
      DO 25 J=1,NP
   25 SD(J)=X(L,J)-T(J)
      CALL RLMLYDBI(SA,SD,NP,NCOV,NP,1)
      CALL RLNRM2BI(SD,NP,1,NP,ZNR)
      DISTL=ZNR
      IF (ICNV.EQ.2) DELTA=DMAX1(DELTA,DABS(DISTL-DIST(L)))
      DIST(L)=DISTL
 28   CONTINUE
      CALL RLFINDXK(DIST,XC,XM,B0,N,TOL,50,NIT,XK)
      DO 100 L=1,N
      DO  30 J=1,NP
   30 SD(J)=X(L,J)-T(J)
      CALL RLMLYDBI(SA,SD,NP,NCOV,NP,1)
      CALL RLNRM2BI(SD,NP,1,NP,ZNR)
      DISTL=DIST(L)/XK
      U=RLUROCV(DISTL,V,4)
      VRCK=RLVROCV(DISTL,V,4)
      S1=S1+VRCK
      IF (ILOC.EQ.0) GOTO 40
      W=RLWROCV(DISTL,V,4)
      S2=S2+W
   40 IF (IALG.EQ.1) GOTO 60
      UP=RLUPROCV(DISTL,V,4)
      IF (ILOC.EQ.1) S2=S2+RLWPROCV(DISTL,V,4)*ZNR/XP
      IF (IALG.EQ.2) THEN
        S1P=S1P+RLVPROCV(DISTL,V,4)*ZNR
        GOTO 60
      ENDIF
   45 DO 50 I=1,NP
   50 SZ(L,I)=SD(I)
   60 IJ=0
      DO 90 I=1,NP
      IF (ILOC.EQ.1) SR(I)=SR(I)+(X(L,I)-T(I))*W
      DO 90 J=1,I
      IJ=IJ+1
   90 ST(IJ)=ST(IJ)+(SD(I)*U)*SD(J)
      IF (IALG.EQ.1) GOTO 100
      SU(L)=U
      SUP(L)=UP
  100 CONTINUE
      DEN=XN
      IF (IALG.NE.2.AND.DABS(S1).GT.TL) DEN=S1
      DO 110 IJ=1,NCOV
  110 ST(IJ)=ST(IJ)/DEN
      RETURN
      END
C
C----------------------------------------------------------------------
C
      SUBROUTINE RLFINDXK(ZZ,XC,XM,B0,N,TOL,MAXIT,NIT,XKF)
C
C COMPUTES CONSTANT K
C
      implicit double precision (a-h,o-z)
      DIMENSION ZZ(N)
      integer rlisigm2
      EXTERNAL RLRHOT,RLISIGM2
      DATA TL/1.d-10/
C
C  PARAMETER CHECK AND INITIALIZATION
C
      XKB=1.d0
      CONST=B0*DBLE(N)
C
C  STEP 1. SET NIT := 1
C  -------
      NIT=1
C
C  STEP 2. COMPUTE A NEW VALUE XKB FOR XK
C  -------
  100 XK=XKB
      TMP=0.d0
      DO 110 I=1,N
      S=ZZ(I)/XK
  110 TMP=TMP+RLRHOT(S,XC,XM)
      XKB=dSQRT(TMP/CONST)*XK
      IF (XKB.GT.TL) GOTO 300
      XKF=XKB
      CALL RLMESSGE(460,'RLFINDXK',0)
      RETURN
C
C  STEP 3. STOP ITERATIONS IF DESIRED PRECISION IS REACHED
C  -------
  300 IF (RLISIGM2(XK,XKB,TOL).EQ.1.OR.NIT.EQ.MAXIT) GOTO 400
      NIT=NIT+1
      GOTO 100
  400 XKF=XKB
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      FUNCTION RLRHOT(X,XC,XM)
C
C  FUNCTION RHO (BIWEGHT, T-BIWEIGHT, LWS)
C
      implicit double precision (a-h,o-z)
      DIMENSION CST(6)
      DATA NCALL,YC,YM,XC2,XC4,XMPC,XM2,XM3,XM4,CST/0,14*0.d0/
      IF (NCALL.EQ.0.OR.(XC.NE.YC.AND.XM.NE.YM)) THEN
        NCALL=1
        XM2 =XM*XM
        XMPC=XM+XC
        XC2 =XC*XC
        XC4 =XC2*XC2
        XMPC2=XMPC*XMPC
        XM3 =XM2*XM 
        XM4 =XM2*XM2
        CST(1)=XM2/2.d0-XM2*(XM4-5.d0*XM2*XC2+15.d0*XC4)/(30.d0*XC4)
        CST(2)=0.5d0 + XM4/(2.d0*XC4) - XM2/XC2
        CST(3)=4.d0*XM/(3.d0*XC2) - 4.d0*XM3/(3.d0*XC4)
        CST(4)=3.d0*XM2/(2.d0*XC4) - 1.d0/(2.d0*XC2)
        CST(5)=4.d0*XM/(5.d0*XC4)
        CST(6)=1.d0/(6.d0*XC4)
      ENDIF
      X2  =X*X
      RLRHOT=X2/2.d0
      IF (X.LE.XM) RETURN
      IF (X.GT.XM.AND.X.LT.XMPC) THEN
        X3 = X2*X
        X4 = X2*X2
        X5 = X2*X3
        X6 = X3*X3 
        RLRHOT=CST(1)+CST(2)*X2+CST(3)*X3+CST(4)*X4-CST(5)*X5+CST(6)*X6 
        RETURN
      ENDIF
      RLRHOT=XM2/2.d0+XC*(5.d0*XC+16.d0*XM)/30.d0
      RETURN
      END
C
C-----------------------------------------------------------------------
C     
      FUNCTION RLUROCV(S,V,N)
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE U-FUNCTION
C  FOR AFFINE INVARIANT COVARIANCES: ROCKE VERSION
C
      implicit double precision (a-h,o-z)
      dimension V(N)

      XC=V(1)
      XM=V(2)
      RLUROCV=1.D0
      IF (S.LT.XM) RETURN
      RLUROCV=0.D0
      IF (S.GE.XM+XC) RETURN
      ZED=1.d0-((S-XM)/XC)**2
      RLUROCV=ZED**2
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION RLUPROCV(S,V,N)
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERIVATIVE
C  OF THE U-FUNCTION FOR AFFINE INVARIANT COVARIANCES: ROCKE VERSION
C
      implicit double precision (a-h,o-z)
      dimension V(N)
      XC=V(1)
      XM=V(2)
      RLUPROCV=0.D0
      IF (S.LE.XM.OR.S.GE.XM+XC) RETURN
      Z2=XC**2
      Q=XM-S
      RLUPROCV=-4.D0*(Q**2-Z2)*Q/(Z2**2)
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION RLVROCV(S,V,N)
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE V-FUNCTION
C  FOR AFFINE INVARIANT COVARIANCES: ROCKE VERSION
C
      implicit double precision (a-h,o-z)
      dimension V(N)

      XC=V(1)
      XM=V(2)
      XP=V(3)
      RLVROCV=0.D0
      IF (S.GE.XM+XC) RETURN
      IF (S.GE.0.d0 .AND. S.LE.XM) THEN
         RLVROCV=(S*S)/XP
      ELSEIF (S.GT.XM) THEN
         ZED=S*(1.d0-((S-XM)/XC)**2)
         RLVROCV=ZED**2/XP
      ENDIF
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION RLVPROCV(S,V,N)
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERIVATIVE
C  OF THE V-FUNCTION FOR AFFINE INVARIANT COVARIANCES: ROCKE VERSION
C
      implicit double precision (a-h,o-z)
      dimension V(N)

      XC=V(1)
      XM=V(2)
      XP=V(3)
      RLVPROCV=0.D0
      IF (S.GE.XM+XC) RETURN
      IF (S.GE.0.d0 .AND. S.LE.XM) THEN
         RLVPROCV=2.D0*S/XP
      ELSEIF (S.GT.XM) THEN
         XC2=XC*XC
         ZED=2.d0*S*(1.d0+(XM-3.d0*S)*((XM-S)**3)/(XC2**2)-
     +        2.d0*(XM-S)*(XM-2.d0*S)/XC2)
         RLVPROCV=ZED/XP
      ENDIF
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION RLWROCV(S,V,N)
C
C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE W-FUNCTION
C  FOR AFFINE INVARIANT COVARIANCES: ROCKE VERSION
C
      implicit double precision (a-h,o-z)
      dimension V(N)

      XC=V(1)
      XM=V(2)
      RLWROCV=1.D0
      IF (S.LT.XM) RETURN
      RLWROCV=0.D0
      IF (S.GE.XM+XC) RETURN
      ZED=1.d0-((S-XM)/XC)**2
      RLWROCV=ZED**2
      RETURN
      END
C
C----------------------------------------------------------------------
C
      FUNCTION RLWPROCV(S,V,N)

C  PURPOSE
C  -------
C  GIVES THE VALUE AT THE POINT S OF THE FIRST DERIVATIVE
C  OF THE W-FUNCTION FOR AFFINE INVARIANT COVARIANCES: ROCKE VERSION
C
      implicit double precision (a-h,o-z)
      dimension V(N)

      XC=V(1)
      XM=V(2)
      RLWPROCV=0.D0
      IF (S.LE.XM.OR.S.GE.XM+XC) RETURN
      Z2=XC**2
      Q=XM-S
      RLWPROCV=-4.D0*(Q**2-Z2)*Q/(Z2**2)
      RETURN
      END


C --------Used to be robaux.f-------------------------

      SUBROUTINE RLMESSGE(NUMBER,ITEXT,ISTOP)
C.......................................................................
C
C   COPYRIGHT 1992 Alfio Marazzi
C
C   AUTHOR : A. RANDRIAMIHARISOA
C.......................................................................
C
      CHARACTER *6 ITEXT, CC*34
      IF (ISTOP.EQ.1) THEN
         CC='Input parameter error(s) in '//ITEXT
        CALL XERROR(CC,34,NUMBER,2)
      ELSE
        CALL INTPR(ITEXT,6,NUMBER,1)
      ENDIF
      RETURN
      END

C=======================================================================
      INTEGER FUNCTION RLICNHMC(NVAR,HMAX,H,TOL,ICNV)
C.......................................................................
      DOUBLE PRECISION H(NVAR),HDMAX,HMAX,TOL
C-----------------------------------------------------------------------
C     SUPPORT FUNCTION FOR ITERATIVE ALGORITHM
C     FUNCTION ICNVH TAKEN FROM CVAUXI.F
C-----------------------------------------------------------------------
      RLICNHMC=0
      IF (ICNV.EQ.1) THEN
         CALL RLNRM2BI(H,NVAR,1,NVAR,HDMAX)
         HMAX=HDMAX
      ENDIF
      IF (HMAX.LT.TOL) RLICNHMC=1
      RETURN
      END




