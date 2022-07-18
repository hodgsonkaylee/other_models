/* SIMULATION USING MESSY DATA */

proc import datafile= 'C:/Users/jkaylee/Desktop/STATHomework/STAT666/synvarv01.csv'
	out=work.SyndromeVariables
	dbms=CSV;
run;

title '1-factor CFA - 1% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + e1,
	 y2 = f1 + e2,
	 y3 = lam3 f1 + e3,
	 y4 = lam4 f1 + e4,
	 y5 = lam5 f1 + e5,
	 y6 = lam6 f1 + e6,
	 y7 = lam7 f1 + e7,
	 y8 = lam8 f1 + e8,
	 y9 = lam9 f1 + e9,
	 y10 = lam10 f1 + e10,
	 y11 = lam11 f1 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 = phi1;
  bounds
     0 <= phi1,
	 0 <= psi1-psi11;
run;

title 'First-order 2-factor CFA - 1% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 f2 = phi1 phi2;
  cov
     f1 f2= phi12;
  bounds
     0 <= phi1-phi2,
	 0 <= psi1-psi11;
run;

title 'Second-order 2-factor CFA - 1% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11,
	 f1  = beta1 f3 + d1,
     f2  = beta2 f3 + d2;
  std
     e1-e11= psi1-psi11,
     d1-d2=ksi1-ksi2,
     f3 = phi3;
  bounds
	 0 <= psi1-psi11,
	 0 <= ksi1-ksi2,
     0 <= phi3;
run;


proc import datafile= 'C:/Users/jkaylee/Desktop/STATHomework/STAT666/synvarv05.csv'
	out=work.SyndromeVariables
	dbms=CSV;
run;

title '1-factor CFA - 5% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + e1,
	 y2 = f1 + e2,
	 y3 = lam3 f1 + e3,
	 y4 = lam4 f1 + e4,
	 y5 = lam5 f1 + e5,
	 y6 = lam6 f1 + e6,
	 y7 = lam7 f1 + e7,
	 y8 = lam8 f1 + e8,
	 y9 = lam9 f1 + e9,
	 y10 = lam10 f1 + e10,
	 y11 = lam11 f1 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 = phi1;
  bounds
     0 <= phi1,
	 0 <= psi1-psi11;
run;

title 'First-order 2-factor CFA - 5% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 f2 = phi1 phi2;
  cov
     f1 f2= phi12;
  bounds
     0 <= phi1-phi2,
	 0 <= psi1-psi11;
run;

title 'Second-order 2-factor CFA - 5% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11,
	 f1  = beta1 f3 + d1,
     f2  = beta2 f3 + d2;
  std
     e1-e11= psi1-psi11,
     d1-d2=ksi1-ksi2,
     f3 = phi3;
  bounds
	 0 <= psi1-psi11,
	 0 <= ksi1-ksi2,
     0 <= phi3;
run;

proc import datafile= 'C:/Users/jkaylee/Desktop/STATHomework/STAT666/synvarv10.csv'
	out=work.SyndromeVariables
	dbms=CSV;
run;

title '1-factor CFA - 10% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + e1,
	 y2 = f1 + e2,
	 y3 = lam3 f1 + e3,
	 y4 = lam4 f1 + e4,
	 y5 = lam5 f1 + e5,
	 y6 = lam6 f1 + e6,
	 y7 = lam7 f1 + e7,
	 y8 = lam8 f1 + e8,
	 y9 = lam9 f1 + e9,
	 y10 = lam10 f1 + e10,
	 y11 = lam11 f1 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 = phi1;
  bounds
     0 <= phi1,
	 0 <= psi1-psi11;
run;

title 'First-order 2-factor CFA - 10% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 f2 = phi1 phi2;
  cov
     f1 f2= phi12;
  bounds
     0 <= phi1-phi2,
	 0 <= psi1-psi11;
run;

title 'Second-order 2-factor CFA - 10% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11,
	 f1  = beta1 f3 + d1,
     f2  = beta2 f3 + d2;
  std
     e1-e11= psi1-psi11,
     d1-d2=ksi1-ksi2,
     f3 = phi3;
  bounds
	 0 <= psi1-psi11,
	 0 <= ksi1-ksi2,
     0 <= phi3;
run;

proc import datafile= 'C:/Users/jkaylee/Desktop/STATHomework/STAT666/synvarv25.csv'
	out=work.SyndromeVariables
	dbms=CSV;
run;

title '1-factor CFA - 25% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + e1,
	 y2 = f1 + e2,
	 y3 = lam3 f1 + e3,
	 y4 = lam4 f1 + e4,
	 y5 = lam5 f1 + e5,
	 y6 = lam6 f1 + e6,
	 y7 = lam7 f1 + e7,
	 y8 = lam8 f1 + e8,
	 y9 = lam9 f1 + e9,
	 y10 = lam10 f1 + e10,
	 y11 = lam11 f1 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 = phi1;
  bounds
     0 <= phi1,
	 0 <= psi1-psi11;
run;

title 'First-order 2-factor CFA - 25% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 f2 = phi1 phi2;
  cov
     f1 f2= phi12;
  bounds
     0 <= phi1-phi2,
	 0 <= psi1-psi11;
run;

title 'Second-order 2-factor CFA - 25% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11,
	 f1  = beta1 f3 + d1,
     f2  = beta2 f3 + d2;
  std
     e1-e11= psi1-psi11,
     d1-d2=ksi1-ksi2,
     f3 = phi3;
  bounds
	 0 <= psi1-psi11,
	 0 <= ksi1-ksi2,
     0 <= phi3;
run;

proc import datafile= 'C:/Users/jkaylee/Desktop/STATHomework/STAT666/synvarv50.csv'
	out=work.SyndromeVariables
	dbms=CSV;
run;

title '1-factor CFA - 50% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + e1,
	 y2 = f1 + e2,
	 y3 = lam3 f1 + e3,
	 y4 = lam4 f1 + e4,
	 y5 = lam5 f1 + e5,
	 y6 = lam6 f1 + e6,
	 y7 = lam7 f1 + e7,
	 y8 = lam8 f1 + e8,
	 y9 = lam9 f1 + e9,
	 y10 = lam10 f1 + e10,
	 y11 = lam11 f1 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 = phi1;
  bounds
     0 <= phi1,
	 0 <= psi1-psi11;
run;

title 'First-order 2-factor CFA - 50% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11;
  std
     e1-e11= psi1-psi11,
	 f1 f2 = phi1 phi2;
  cov
     f1 f2= phi12;
  bounds
     0 <= phi1-phi2,
	 0 <= psi1-psi11;
run;

title 'Second-order 2-factor CFA - 50% messy';
proc calis method=uls;
  lineqs
     y1 = lam1 f1 + lam2 f2 + e1,
	 y2 = lam3 f1 + lam4 f2 + e2,
	 y3 = lam5 f1 + lam6 f2 + e3,
	 y4 = lam7 f1 + lam8 f2 + e4,
	 y5 = f1 + e5,
	 y6 = lam9 f1 + lam10 f2 + e6,
	 y7 = f2 + e7,
	 y8 = lam11 f1 + lam12 f2 + e8,
	 y9 = lam13 f1 + lam14 f2 + e9,
	 y10 = lam15 f2 + e10,
	 y11 = lam16 f1 + lam17 f2 + e11,
	 f1  = beta1 f3 + d1,
     f2  = beta2 f3 + d2;
  std
     e1-e11= psi1-psi11,
     d1-d2=ksi1-ksi2,
     f3 = phi3;
  bounds
	 0 <= psi1-psi11,
	 0 <= ksi1-ksi2,
     0 <= phi3;
run;
