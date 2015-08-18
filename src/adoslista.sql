--CASHFLOW_LISTA
/*
SELECT * FROM INTERVALLUM
where tipus = 2
ORDER BY ALSO ;

SELECT DISTINCT C.R_UGYFEL,
				MAX(C.R_BESOROLASERTEK) AS R_BESOROLASERTEK,
				SUM(C.R_OSSZEG) AS R_OSSZEG,
				MAX(U.NEV) AS NEV,
				MAX(C.R_BESOROLASNEV) AS BESOROLASTETEL
FROM CASHFLOW_LISTA( 1 , '2015-07-01' , 0, 0 , 1 , -1 ,0) C, UGYFEL U
WHERE C.R_UGYFEL = U.ID_UGYFEL
GROUP BY C.R_UGYFEL;
*/

--
create or alter procedure CASHFLOW_LISTA (
	P_TIPUS smallint,
	P_TARGYNAP date,
	P_UGYFEL integer,
	P_DEVIZA smallint,
	P_ALAPDATUM smallint,
	P_UGYFELBESOROLAS integer,
	P_INTERVALLUM integer)
returns (
	R_INTERVALLUM smallint,
	R_BIZONYLAT integer,
	R_UGYFEL integer,
	R_OSSZEG double precision,
	R_NAPSZAM smallint,
	R_FIZHATAR date,
	R_FIZDATUM date,
	R_IKTATOSZAM varchar(20),
	R_BESOROLASERTEK integer,
	R_BESOROLASNEV varchar(40))
as
declare variable V_FORINTOSSZEG double precision;
declare variable V_DEVIZAOSSZEG double precision;
declare variable V_FIZETVE smallint;
declare variable V_IRANY smallint;
declare variable V_SZORZO smallint;
declare variable V_FOKONYV varchar(10);
declare variable V_DATUM date;
declare variable V_FIZHATAR date;
declare variable V_BIZSZAM varchar(30);
declare variable V_TARTOSSZEG double precision;
declare variable V_KOVOSSZEG double precision;
declare variable V_ARFBIZONYLAT smallint;
declare variable V_UGYFELBESOROLAS2 integer;
begin
	if (P_TIPUS = 1) then
		V_SZORZO = 1;
	else
		V_SZORZO = -1;

	if (P_TIPUS = 1) then
		V_FOKONYV = '31';
	else
		V_FOKONYV = '45';
	for select B.ID_BIZONYLAT,
				(B.OSSZEG - B.FIZOSSZEG),
				(B.DEVIZA - B.FIZDEVIZA), B.FIZETVE, B.DATUM, B.FIZHATAR, B.FIZDATUM, B.ID_UGYFEL, B.IKTATOSZAM, B.IRANY
		from BIZONYLAT B join
			 SZAMLAREND S on B.ID_SZAMLAREND = S.ID_SZAMLAREND
		where (S.SZAMLASZAM starting with :V_FOKONYV) and
				ID_CSOPORT in (select ID_CSOPORT
								from CSOPORT
								where TELJESITENDO = 1
							) and
				((B.FIZETVE = 0) or (B.FIZETVE = 1 and
						((B.OSSZEG <> B.FIZOSSZEG) or (B.DEVIZA <> B.FIZDEVIZA)))) and
				(:P_UGYFEL = 0 or B.ID_UGYFEL = :P_UGYFEL) and
				(:P_DEVIZA = 0 or B.ID_DEVIZA = :P_DEVIZA) and
				(
					(:P_UGYFELBESOROLAS = 0 or :P_UGYFELBESOROLAS = -1) or (
						(
							select first 1 ID_HIVATKOZAS
							from BESOROLASERTEK
							where ID_BESOROLASTETEL = :P_UGYFELBESOROLAS and
							ID_HIVATKOZAS = B.ID_UGYFEL
						) > 0
					)
				)
		into :R_BIZONYLAT, :V_FORINTOSSZEG, :V_DEVIZAOSSZEG, :V_FIZETVE, :V_DATUM, :V_FIZHATAR, :R_FIZDATUM, :R_UGYFEL,
				:R_IKTATOSZAM, :V_IRANY
	do begin
		R_BESOROLASERTEK = 0;
		R_BESOROLASNEV = '';
		if (:P_UGYFELBESOROLAS = -1) then
			select first 1 BE.ID_BESOROLASTETEL, BT.NEV
			from BESOROLASERTEK BE, BESOROLAS B, BESOROLASTETEL BT
			where BE.ID_HIVATKOZAS = :R_UGYFEL and
				  BE.ID_BESOROLASTETEL = BT.ID_BESOROLASTETEL and
				  BT.ID_BESOROLAS = B.ID_BESOROLAS and
				  B.TIPUS = 2
			into :R_BESOROLASERTEK, :R_BESOROLASNEV;
		if (R_BESOROLASERTEK is null) then
			R_BESOROLASERTEK = 0;
		if (R_FIZDATUM = '1899.12.30') then
			R_FIZDATUM = P_TARGYNAP;
		if (V_FIZHATAR = '1899.12.30') then
			V_FIZHATAR = P_TARGYNAP;
		if (P_ALAPDATUM = 0) then
			R_FIZHATAR = V_DATUM;
		else
			R_FIZHATAR = V_FIZHATAR;
		if (R_FIZDATUM is null) then
			R_FIZDATUM = P_TARGYNAP;
		if ((V_FORINTOSSZEG < 0) or (V_DEVIZAOSSZEG < 0)) then begin
			R_NAPSZAM = R_FIZHATAR - P_TARGYNAP;
		end
		else begin
			R_NAPSZAM = R_FIZHATAR - P_TARGYNAP;
		end
		if (P_DEVIZA = 0) then
			R_OSSZEG = V_FORINTOSSZEG * V_SZORZO * V_IRANY;
		else
			R_OSSZEG = V_DEVIZAOSSZEG * V_SZORZO * V_IRANY;

		V_ARFBIZONYLAT = 0;
		R_INTERVALLUM = null;

		select min(ID_INTERVALLUM)
		from INTERVALLUM
		where :R_NAPSZAM >= ALSO and
			  :R_NAPSZAM <= FELSO and
			  TIPUS = 2
		into :R_INTERVALLUM;

		if ((R_INTERVALLUM is not null) and
			(V_ARFBIZONYLAT = 0)) then begin
			if (P_INTERVALLUM = 0) then
				suspend;
			else if (P_INTERVALLUM = R_INTERVALLUM) then
				suspend;
			end
		end

		if (:P_UGYFELBESOROLAS = -1) then
			V_UGYFELBESOROLAS2 = 0;
		else
			V_UGYFELBESOROLAS2 = P_UGYFELBESOROLAS;

		R_BESOROLASERTEK = 0;
		R_BESOROLASNEV = '';
		for 
			select UB.R_UGYFEL, UB.R_FIZDATUM, UB.R_BIZSZAM, UB.R_IKTATOSZAM, UB.R_TARTOSSZEG, UB.R_KOVOSSZEG
			from UGYFEL_BANKTETEL(:P_TIPUS, :P_UGYFEL, null, current_date, 0, :P_DEVIZA, current_date, :V_FOKONYV, '', 0, :V_UGYFELBESOROLAS2) UB, UGYFEL U
			where UB.R_UGYFEL = U.ID_UGYFEL and
				  (:P_UGYFEL = 0 or UB.R_UGYFEL = :P_UGYFEL)
			into :R_UGYFEL, :R_FIZDATUM, :V_BIZSZAM, :R_IKTATOSZAM, :V_TARTOSSZEG, :V_KOVOSSZEG
		do begin
			R_BESOROLASERTEK = 0;
			R_BESOROLASNEV = '';
			if (:P_UGYFELBESOROLAS = -1) then
				select first 1 BE.ID_BESOROLASTETEL, BT.NEV
				from BESOROLASERTEK BE, BESOROLAS B, BESOROLASTETEL BT
				where BE.ID_HIVATKOZAS = :R_UGYFEL and
					  BE.ID_BESOROLASTETEL = BT.ID_BESOROLASTETEL and
					  BT.ID_BESOROLAS = B.ID_BESOROLAS and
					  B.TIPUS = 2
				into :R_BESOROLASERTEK, :R_BESOROLASNEV;
			if (V_TARTOSSZEG <> 0) then begin
				V_IRANY = 1;
				V_FORINTOSSZEG = V_TARTOSSZEG;
			end
			else begin
				V_IRANY = -1;
				V_FORINTOSSZEG = V_KOVOSSZEG;
			end
			R_NAPSZAM = R_FIZDATUM - P_TARGYNAP;
			if (P_DEVIZA = 0) then
				R_OSSZEG = V_FORINTOSSZEG * V_SZORZO * V_IRANY;
			else
				R_OSSZEG = V_DEVIZAOSSZEG * V_SZORZO * V_IRANY;

			R_INTERVALLUM = null;
			select min(ID_INTERVALLUM)
			from INTERVALLUM
			where :R_NAPSZAM >= ALSO and
				  :R_NAPSZAM <= FELSO and
				  TIPUS = 2
			into :R_INTERVALLUM;
			if (R_INTERVALLUM is not null) then begin
				if (P_INTERVALLUM = 0) then
					suspend;
				else if (P_INTERVALLUM = R_INTERVALLUM) then
					suspend;
			end
		end
	end
