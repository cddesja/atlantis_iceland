Option Explicit

Sub CreateVertNumsCDF()
#This script creates a .txt file with initial vert numbers at age by box, in the same directory as this .xls file

numsheets <- NULL
group <- NULL
box <- NULL
numboxes <- NULL
numages <- NULL
numgroups <- NULL
fnum <- NULL
numstring <- NULL
atlantisname <- NULL
currentxlsname <- NULL
outputfilename <- NULL
pathname <- NULL
nums <- NULL
fill1 <- NULL
fill2 <- NULL
lastline <- NULL
numsatage <- NULL
namefile <- NULL
start <- NULL
strlngth <- NULL
r <- NULL

Dim numsheets As Long
Dim group, age, box, numboxes, numages, numgroups, fnum As Integer
Dim numstring, atlantisname As String
Dim currentxlsname, outputfilename, pathname As String
Dim nums, fill1, fill2, lastline As String
Dim numsatage(), namefile, start As String
Dim strlngth As String
Dim r As Integer

'----------------------------------------------
'Below are things you may want to change
currentxlsname = "ECCALbioparams.xls" 'name of this excel file
outputfilename = "VertNums_for_CDF.txt" 'name of the file you want outputted
numages = 10  'number of age classes, should always be 10
numboxes = 82 'number of 2-d boxes in the model
start = "g60" 'address of the cell one row up and one column left of where numbers of age1 in box 0 begin

'------------------------------------------------


fill1 = ", _, _, _, _, _, _, _," 'formatting for the 7 depth layers after the one that numbers go into
fill2 = " =" 'formatting that gets added to the group-age class name in the cdf
lastline = "  _, _, _, _, _, _, _, _ ;" 'formatting for the last line of each age class entry

numgroups = Worksheets.Count
fnum = FreeFile() 'retrives the next available file number
pathname = Workbooks(currentxlsname).Path
namefile = pathname & "\" & outputfilename

Open namefile For Output As fnum 'opens output file


ReDim numsatage(numboxes, 1) 'dimensions an array with the number of boxes in the model for output


For group = 1 To numgroups
    If Len(Worksheets(group).Name) < 4 Then 'for all sheets with 3 letter code names do the following:
        
    For age = 1 To numages
        atlantisname = Worksheets(group).Range(start).Offset(0, age) 'get the atlantis cdf name from the sheet
    
        For box = 1 To numboxes 'run through all the boxes
            nums = Sheets(group).Range(start).Offset(box, age) 'get numbers at age for this box
            If nums = 0 Then 'if there isn't anything in the box, use the following text string
            numstring = "_, _, _, _, _, _, _, _,"
            Else 'otherwise put everything in the first depth layer and fill in the rest with the correct formatting
            numstring = nums & fill1
            End If
            numsatage(box, 1) = numstring 'fills an array with an entry for each box
        Next box
        
        'prints "atlantis name for cdf ="
        Print #fnum, " " & atlantisname & fill2
        
        
        'Commented out by PH to output only one set of values 9/15/08
        'For box = 1 To numboxes
        '    Print #fnum, numsatage(box, 1) 'prints the numbers at age per box on consecutive new lines
        'Next box
            
        For box = 1 To numboxes - 1
            Print #fnum, "  " & numsatage(box, 1) 'repeats the previous loop, except for the very last entry
        Next box
            
        Print #fnum, lastline 'creates the last entry for the age-class, with a ; ending the line
        Print #fnum, 'prints a blank line
        
        'ADDED BY PJH SEP 18, 2008 TO CREATE DEFAULT MATRICES FOR _RESN AND _STRUCN and _N
        'PRINTS 82 LINES OF DEFAULT
        strlngth = Len(atlantisname)
        Print #fnum, " " & Left(atlantisname, strlngth - 4) & "StructN" & fill2
        For r = 1 To 81
        
            Print #fnum, "  _, _, _, _, _, _, _, _,"
       
        Next
       
        Print #fnum, "  _, _, _, _, _, _, _, _ ;"
        Print #fnum, 'prints a blank line
       
        Print #fnum, " " & Left(atlantisname, strlngth - 4) & "ResN" & fill2
                For r = 1 To 81
       
            Print #fnum, "  _, _, _, _, _, _, _, _,"
       
        Next
       
        Print #fnum, "  _, _, _, _, _, _, _, _ ;"
        Print #fnum, 'prints a blank line
        
        If age = 1 Then
            Print #fnum, " " & Left(atlantisname, strlngth - 6) & "_N" & fill2
            For r = 1 To 81
       
                Print #fnum, "  _, _, _, _, _, _, _, _,"
       
            Next
       
            Print #fnum, "  _, _, _, _, _, _, _, _ ;"
            Print #fnum, 'prints a blank line
        End If
        '''''

        
        
    Next age
    
    
    End If
Next group

Close #fnum 'closes the file, must do this or else you can't open it again
End Sub

