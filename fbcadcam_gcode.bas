Sub gcode_create_gcode()
	Dim As Integer i,j,grouped
	glinesc=0
	ReDim glines(glinesc,4)
	If glines_group=TRUE Then
		grouped=1
	Else
		grouped=0
	EndIf
	For i = 1 To linec
		If lines(i,8)=grouped Then
			glinesc+=1
			ReDim Preserve glines(glinesc,4)
			glines(glinesc,1)=lines(i,1)
			glines(glinesc,2)=lines(i,2)
			glines(glinesc,3)=lines(i,4)
			glines(glinesc,4)=lines(i,5)
		EndIf
	Next
	For i=1 To circlec
		If circles(i,10)=grouped Then
			Select Case circles(i,9)
				Case 1
					gcode_plotellipse(circles(i,1),circles(i,2),circles(i,4),0,360,circles(i,4),0)
				Case 2
					gcode_plotellipse(circles(i,1),circles(i,2),circles(i,4),circles(i,6)*r2d,circles(i,7)*r2d,circles(i,4),0)
				Case 3,4
					gcode_plotellipse(circles(i,1),circles(i,2),circles(i,4),circles(i,6),circles(i,7),circles(i,8),circles(i,11))
			End Select
		EndIf
	Next

	gcode_remove_duplicates
	gcode_sort_the_glines
	gcode_generate_g_code
	gseq=0
	
	'gcode_create_fbcadcam_lines
End Sub

Sub gcode_create_fbcadcam_lines()
	Dim As Integer gcode_line_color = 14
	For i As Integer = 1 To glinesc
		memmanageline
		lines(linec,1)=glines(i,1)
		lines(linec,2)=glines(i,2)
		lines(linec,4)=glines(i,3)
		lines(linec,5)=glines(i,4)
		lines(linec,7)=gcode_line_color
		If gcode_line_color=14 Then
			gcode_line_color=13
		Else
			gcode_line_color=14
		EndIf
	Next
End Sub

Sub gcode_remove_duplicates
	Dim As Integer tempc
	Dim As BOOLEAN dupe
	Dim As Double temp(glinesc,4)
	For i As Integer = 1 To glinesc
		dupe=FALSE
		For j As Integer = i+1 To glinesc
			If glines(i,1)=glines(j,1) And glines(i,2)=glines(j,2) And glines(i,3)=glines(j,3) And glines(i,4)=glines(j,4) Then
				dupe=TRUE
				Exit For
			EndIf
		Next
		If dupe=FALSE Then
			tempc+=1
			temp(tempc,1)=glines(i,1)
			temp(tempc,2)=glines(i,2)
			temp(tempc,3)=glines(i,3)
			temp(tempc,4)=glines(i,4)
		EndIf
	Next
	If tempc<glinesc Then
		'reset glines array
		glinesc=tempc
		ReDim glines(glinesc,4)
		For i As Integer=1 To glinesc
			glines(i,1)=temp(i,1)
			glines(i,2)=temp(i,2)
			glines(i,3)=temp(i,3)
			glines(i,4)=temp(i,4)
		Next
	EndIf

End Sub

Sub gcode_plotellipse(ex As Double,ey As Double,er1 As Double,estart As Double,eend As Double,er2 As Double,eangle As Double)
	Dim As Double i,x1p,y1p
	Dim As Double ax,ay,bx,by,endx,endy,easpect,eresolution,elength
	Dim As BOOLEAN eplot
	If estart>eend Then
		elength=360+eend
	Else
		elength=eend
	EndIf
	eplot=FALSE
	eresolution=gcode_curve_resolution
	ax=Cos(estart*d2r)*er1*Cos(eangle*d2r) - Sin(estart*d2r)*er2*Sin(eangle*d2r)+ex
	ay=Cos(estart*d2r)*er1*Sin(eangle*d2r) + Sin(estart*d2r)*er2*Cos(eangle*d2r)+ey
	endx=Cos(eend*d2r)*er1*Cos(eangle*d2r) - Sin(eend*d2r)*er2*Sin(eangle*d2r)+ex
	endy=Cos(eend*d2r)*er1*Sin(eangle*d2r) + Sin(eend*d2r)*er2*Cos(eangle*d2r)+ey
	For i = estart+eresolution To elength Step eresolution
		bx=Cos(i*d2r)*er1*Cos(eangle*d2r) - Sin(i*d2r)*er2*Sin(eangle*d2r)+ex
		by=Cos(i*d2r)*er1*Sin(eangle*d2r) + Sin(i*d2r)*er2*Cos(eangle*d2r)+ey
		glinesc+=1
		ReDim Preserve glines(glinesc,4)
		glines(glinesc,1)=ax
		glines(glinesc,2)=ay
		glines(glinesc,3)=bx
		glines(glinesc,4)=by
		ax=bx
		ay=by
	Next
	If bx<>endx And by<>endy Then
		bx=endx
		by=endy
		glinesc+=1
		ReDim Preserve glines(glinesc,4)
		glines(glinesc,1)=ax
		glines(glinesc,2)=ay
		glines(glinesc,3)=bx
		glines(glinesc,4)=by
	EndIf
End Sub

Sub gcode_generate_g_code()
	Dim As Double x1,y1,x2,y2,px2,py2,gcode_z_axis_plunge_depth_step
	Dim As String gcode_file_name,gcode_file_name_datetime
	Dim As Integer gcodeff
	Dim As String t,tt
	t=Time
	For i As Integer=1 To Len(t)
		If Mid(t,i,1)=":" Then
			tt+="."
		Else
			tt+=Mid(t,i,1)
		EndIf
	Next
	gcode_file_name_datetime=Date & "-" & tt
	gcodeff=FreeFile
	If openeddrawingname="" Then
		gcode_file_name="untitled_" & gcode_file_name_datetime & ".ngc"
	Else
		gcode_file_name=Mid(openeddrawingname,1,Len(openeddrawingname)-4) & "_" & gcode_file_name_datetime & ".ngc"
	EndIf
	Open gcode_file_name For Output As #gcodeff
	Select Case gcode_units
		Case 20
			Print #gcodeff, "G20
		Case 21
			Print #gcodeff, "G21"
		case Else'default is mm
			Print #gcodeff, "G21"
	End Select
	Print #gcodeff, "G90
	Print #gcodeff, "G0 X0 Y0"
	For plungez As Integer = gcode_Z_axis_steps To 1 Step -1
		gcode_z_axis_plunge_depth_step=gcode_z_axis_plunge_depth/plungez
		For i As Integer = 1 To glinesc
			x1=glines(i,1)
			y1=glines(i,2)
			x2=glines(i,3)
			y2=glines(i,4)
			If Abs(x1-px2)>.00001 Or Abs(y1-py2)>.00001 Or i=1 Then
				'rapid pick up the bit
				Print #gcodeff, "G0 Z0"
				'rapid move to first line to cut
				Print #gcodeff, "G0 X" & x1 & " Y" & y1
				'slowly move the bit down
				Print #gcodeff, "G1 Z" & Str(gcode_z_axis_plunge_depth_step) & " F" & Str(gcode_z_feed_rate)
				'set the xy feed rate for cutting
				Print #gcodeff, "G1 F" & Str(gcode_xy_feed_rate)
			EndIf
			px2=x2
			py2=y2
			Print #gcodeff, "G1 X" & x2 & " Y" & y2
		Next
	Next
	Print #gcodeff, "G0 Z0"
	Print #gcodeff, "G0 X0 Y0"
	Close #gcodeff
	theboxbelow("G-Code saved to file")
End Sub

Sub gcode_showpath()
	Dim As Integer gcirsize
	gcirsize=Int((wx2-wx1)/100)
	Select Case gseq
		Case 0
			glength=Sqr(glines(1,1)^2 + glines(1,2)^2)
			gseq=1
			gj=0
			gi=0
		Case 1'start from home and go to first line
			gx=glines(1,1)/glength*gj
			gy=glines(1,2)/glength*gj
			Circle(gx,gy),gcirsize,14
			gj+=2
			If gj>glength Then gseq=2
		Case 2
			gi+=1
			gj=0
			glength=Sqr((glines(gi,1)-glines(gi,3))^2 + (glines(gi,2)-glines(gi,4))^2)
			gseq=3
		Case 3'go thru a lines
			gx=glines(gi,1)+(glines(gi,3)-glines(gi,1))/glength*gj
			gy=glines(gi,2)+(glines(gi,4)-glines(gi,2))/glength*gj
			Circle(gx,gy),gcirsize,15
			For gpasses As Integer = 1 To gseqn
				Circle(gx,gy),gcirsize+gpasses*2,14
			Next
			gj+=1
			If gj>glength Then
				If gi=glinesc Then
					gseqn+=1
					If gseqn=gcode_Z_axis_steps Then'go home
						glength=Sqr(glines(gi,3)^2 + glines(gi,4)^2)
						gj=0
						gseq=5
					Else'do it one more time
						glength=Sqr((glines(1,1)-glines(gi,3))^2 + (glines(1,2)-glines(gi,4))^2)
						gj=0
						gseq=6
					EndIf
				Else
					glength=Sqr((glines(gi+1,1)-glines(gi,3))^2 + (glines(gi+1,2)-glines(gi,4))^2)
					If glength>.01 Then
						gj=0
						gseq=4
					Else
						gseq=2
					EndIf
				EndIf
			EndIf
		Case 4'go from line to line
			gx=glines(gi,3)+(glines(gi+1,1)-glines(gi,3))/glength*gj
			gy=glines(gi,4)+(glines(gi+1,2)-glines(gi,4))/glength*gj
			Circle(gx,gy),gcirsize,14
			gj+=2
			If gj>glength Then gseq=2
		Case 5'go home
			gx=glines(glinesc,3)+(0-glines(glinesc,3))/glength*gj
			gy=glines(glinesc,4)+(0-glines(glinesc,4))/glength*gj
			Circle(gx,gy),gcirsize,14
			gj+=2
			If gj>glength Then
				selbutton=141
				turnbuttonoff
			EndIf
		Case 6'go back to first line
			gx=glines(gi,3)+(glines(1,1)-glines(gi,3))/glength*gj
			gy=glines(gi,4)+(glines(1,2)-glines(gi,4))/glength*gj
			Circle(gx,gy),gcirsize,14
			gj+=2
			If gj>glength Then
				gseq=2
				gi=0
			EndIf
	End Select
End Sub

Sub gcode_sort_the_glines()
	Dim As Integer i,c,startline,nextline
	Dim As Double dist,cx,cy
	cx=0
	cy=0
	startline=1
	c=1
	dist=1000000'set distance to an arbitrary large value
	'find the nearest line to the cnc home position (0,0)
	'and start there
	For i = 1 To glinesc
		If (glines(i,1)-cx)^2+(glines(i,2)-cy)^2 < dist Or (glines(i,3)-cx)^2+(glines(i,4)-cy)^2 < dist Then
			startline=i
			If (glines(i,1)-cx)^2+(glines(i,2)-cy)^2 < (glines(i,3)-cx)^2+(glines(i,4)-cy)^2 Then
				dist=(glines(i,1)-cx)^2+(glines(i,2)-cy)^2
			Else
				dist=(glines(i,3)-cx)^2+(glines(i,4)-cy)^2
			EndIf
		EndIf
	Next
	'now i find which point of the line is nearst to cx,cy: either POINT A or POINT B
	'if point B < point A then
	i=startline
	glines(i,0)=c'here i use element zero to hold the lines sorted order
	If (glines(i,3)-cx)^2+(glines(i,4)-cy)^2 < (glines(i,1)-cx)^2+(glines(i,2)-cy)^2 Then
		'swap the points
		Swap glines(i,1),glines(i,3)
		Swap glines(i,2),glines(i,4)
	Else
		'do nothing
	EndIf
	'now try to find any other lines that are nearst to POINT B of our starting line
	'only check those lines whose element zero has not been assigned a value yet
	'first set the values of cx and cy to point B
	cx=glines(i,3)
	cy=glines(i,4)
	Do While c<glinesc
		dist=1000000'set distance to an arbitrary large value
		For i = 1 To glinesc
			If glines(i,0)=0 Then
				If (glines(i,1)-cx)^2+(glines(i,2)-cy)^2 < dist Or (glines(i,3)-cx)^2+(glines(i,4)-cy)^2 < dist Then
					nextline=i
					If (glines(i,1)-cx)^2+(glines(i,2)-cy)^2 < (glines(i,3)-cx)^2+(glines(i,4)-cy)^2 Then
						dist=(glines(i,1)-cx)^2+(glines(i,2)-cy)^2
					Else
						dist=(glines(i,3)-cx)^2+(glines(i,4)-cy)^2
					EndIf
				EndIf
			EndIf
		Next
		c+=1
		i=nextline
		glines(i,0)=c'here i use element zero to hold the lines sorted order
		If (glines(i,3)-cx)^2+(glines(i,4)-cy)^2 < (glines(i,1)-cx)^2+(glines(i,2)-cy)^2 Then
			'swap the points
			Swap glines(i,1),glines(i,3)
			Swap glines(i,2),glines(i,4)
		Else
			'do nothing
		EndIf
		cx=glines(i,3)
		cy=glines(i,4)
	Loop
	'now rearrang the array 'we can use a nice quick sort for this
	'but for now i will just use a temporary array
	
	Dim As Double temparray(glinesc,4)
	Dim As Integer tempc=1
	Do
		For i=1 To glinesc
			If glines(i,0)=tempc Then
				temparray(tempc,1)=glines(i,1)
				temparray(tempc,2)=glines(i,2)
				temparray(tempc,3)=glines(i,3)
				temparray(tempc,4)=glines(i,4)
				If tempc=glinesc Then Exit Do 'all done
				tempc+=1
				Exit For
			EndIf
		Next
	Loop
	
	'now copy temparray to glines array
	For i=1 To glinesc
		glines(i,1)=temparray(i,1)
		glines(i,2)=temparray(i,2)
		glines(i,3)=temparray(i,3)
		glines(i,4)=temparray(i,4)
	Next


End Sub

