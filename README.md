# ISO8583_cobol_utility
COBOL application to deblock and build an ISO8583 message


## SCOPE OF THE APPLICATION
This application can be used to manage an ISO8583 string message.
Two are the main use cases supported:
•	DEBLOCK phase: Extract a list of details from a given message.
•	INBLOCK phase: Build a message from a given list of details.

This application encapsulate all the logics related to bitmaps, fixed and variable data elements, lengths for variable values and TLV structures


## NAMING CONVENTIONS
There is a naming convention in order to recognize which phase a component belongs to:
If the 4th byte is D the component is used for DEBLOCK phase.
If the 4th byte is I the component is used for INBLOCK phase.
In all the other case the component is used in both phases.


## LIST OF MODULES
Following the list of the modules that compose the application:
**COPYBOOKS** 
X60MCFMT	standard format copybook
X60MCP	parameters copybook
X60MCR	result copybook
X60MCSP	copybook useful to redefine special programs
X60MCSTR	ISO8583 message structure
X60MIO	input/output copybook
X60D002I	input copybook for DEBLOCK of subdefined elements	
X60D002O	output copybook for DEBLOCK of subdefined elements
X60I002I	input copybook for INBLOCK of subdefined elements
X60I002O	output copybook for INBLOCK of subdefined elements
**PROGRAMS**
X60D001	DEBLOCK principal program
X60D002	DEBLOCK TLV elements program
X60I001	INBLOCK principal program
X60I002	INBLOCK TLV elements program

## ISO MESSAGE STRUCTURE
Copybook X60MCSTR contains the supported STRUCTURES (or version) of the message.
This MUST be selected through a dedicated input parameter.

### STRUCTURE RECORD
The STRUCTURE RECORD is made by 3 fields:
descriptor: data element
format: {F: fixed, V: variable}
length:	for FIXED data elements -> fixed length of the value
	for VARIABLE data elements -> bytes occupied by the length descriptor


## PARAMETERS
Copybook X60MCP contains the parameters used by the application to modify its behavior.
VERSION parameter is mandatory
FORMAT OVERRIDE TABLE is optional


## FORMAT
ISO8583 message supports subdefined TLV data elements (Tag-Length-Value).
Through the copybook X60MCFMT this format can be provided  as a list of FORMAT RECORDS
A FORMAT RECORD is made up by 4 fields:
data_element: the data element subdefined. It MUST be a number
hex_conversion_flag: used to support the conversion from zoned value to hexadecimal value and viceversa.
To enable the conversion this field must be set to H
subdefinition_type: describes how the data element is subdefined. It MUST be set to TLV for TLV data elements, and SPC (special) for non TLV data elements
pattern: 4 bytes rule that describes the TLV structure.
	1: TLV Tag format -> {C: characher, H: hexadecimal}
	2: TLV Tag length -> bytes occupied by tag portion
	3: TLV Length format -> {C: character, H: hexadecimal}
	4: TLV Length length -> bytes occupied by length portion

A TLV element can be a TVL itself, so patterns can be further subdefined with the same 4 bytes structure:
A TLV with 2 bytes tag and 3 bytes length contains a TLV with 1 byte tag and 1 bye length:
	C2C3C1C1
For NON TLV data elements the pattern contains the name of the special program used to support DEBLOCK and INBLOCK.
Copybook X60MCF01 contains the format for INCAS protocol.
Copybook X60MCF02 contains the format for STRATUS protocol.

### SPECIAL PROGRAMS
These are programs used to support the DEBLOCK and INBLOCK phases for NON TLV data elements.
In order to be aligned with the naming convention the program name MUST be X60_Snn.
Where the underscore will be changed during DEBLOCK and INBLOCK phase in D or I accordingly.


## FORMAT OVERRIDE
The optional parameter FORMAT OVERRIDE TABLE can be provided in order to add or modify a data element format record.


## DEBLOCK INTEGRATION
DEBLOCK phase is processed by X60D001.
The message to be deblocked must be set in MIO-ISO-MESSAGE
The format can be optionally specified, if no format is specified the deblock will align to the structure.
The mandatory parameter MP-VERSION MUST be set.
The optional parameter FORMAT OVERRIDE TABLE can be used to personalize the format to be applied.

Example of call:
```cobol
     SET MP-VERSION-87       TO TRUE.
     MOVE my_message_format  TO FMT-MSG
     MOVE my_iso_message     TO MIO-ISO-MESSAGE

     CALL X60D001 USING MIO FMT-MSG MP MR
```

The list of the details is provided in MIO-DETAILS


## INBLOCK INTEGRATION
INBLOCK phase is processed by X60I001.
The list of the details MUST be set in MIO-DETAILS
The format can be optionally specified, if no format is specified the deblock will align to the structure.
The mandatory parameter MP-VERSION MUST be set.
The optional parameter FORMAT OVERRIDE TABLE can be used to personalize the format to be applied.

Example of call:
```cobol
     SET MP-VERSION-87       TO TRUE.
     MOVE my_message_format  TO FMT-MSG

* DE002
     ADD 1         TO MIO-DETAILS-TOT
     MOVE '002'    TO MIO-DETAIL-LABEL      (MIO-DETAILS-TOT)
     MOVE 16       TO MIO-DETAIL-VALUE-LEN  (MIO-DETAILS-TOT)
     MOVE '5555555555555555'
                   TO MIO-DETAIL-VALUE-DATA (MIO-DETAILS-TOT).

* DE003
     ADD 1         TO MIO-DETAILS-TOT
     MOVE '003'    TO MIO-DETAIL-LABEL      (MIO-DETAILS-TOT)
     MOVE 6        TO MIO-DETAIL-VALUE-LEN  (MIO-DETAILS-TOT)
     MOVE '280000'
                   TO MIO-DETAIL-VALUE-DATA (MIO-DETAILS-TOT).

CALL X60I001 USING MIO FMT-MSG MP MR
```

The message is provided in MIO-ISO-MESSAGE


## RESULT
Copybook X60MCR contains a simple structure to check the result of the execution.
For NON ZERO result code, a description of the error condition is set.
If the error is raised by a detail, this is returned in position field


## MAINTENANCE
New structures can be easily added to X60MCSTR copybook in order to support other versions of ISO8583.
To support new versions paragraph SET-ISO-MSG-VERSION in X60D001 and X60I001 must be changed accordingly.
New data elements formats can be supported by special programs that can be added to this application.

Any format anomaly can be avoided through the possibility of override it. 
