data temp;
	do mode= "I" ,"C" ,"S";
		do temperature = 1 to 4;
			do rep = 1 to 2;
				input y @@;
				output;
			end;
		end;
	end;
datalines;
12 16 15 19 31 39 53 55 15 19 17 17 30 34 51 49 11 17 24 22 33 37 61 67
;

proc glm;
	title "Mixed Effect Model";
	class temperature mode;
	model y = temperature|mode;
	random temperature mode*temperature /test; *SAS computes for mixed effect not all divided by MSE, correct F values;
run;
quit;

