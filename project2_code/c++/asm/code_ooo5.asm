	ADDI R1 R1 8
	SWS F1 0(R1)
	LWS F2 0(R2)
	ADDS F3 F1 F2
	SWS F3 4(R1)
	SWS F2 0(R1)
	LWS F4 4(R1)
	LWS F5 0(R1)
	MULTS F6 F4 F5
	SWS F6 0(R1)
	EOP