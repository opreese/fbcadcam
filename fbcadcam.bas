'Author Owen P. Reese fbcadcam.com/net/org opreese@gmial 407-655-8537
'integrating fbcadcam with gtk3 GNU LIBRARY GENERAL PUBLIC LICENSE
'fbcadcam version 2019-02-26

'credits
'Draw entities/shapes and snap Icons by Lise Griffith @ wwww.bythepixel.net
'Other Icons are by Shela Reese shelareese5@gmail.com

'A Freebasic.net Project FBCADCAM (AKA FBCAD) since 2007

'Update: October 2018
'This project's websites (fbcadcam.com, fbcadcam.net and fbcadcam.org)
'Oerations relocated to the Philippines
'Door 12 RJD Bldg.
'Blk. 2 Lot 34 Tandoc Ave.
'Pecsonville,
'Tungkong Mangga,
'CSJDM, BULACAN
'Philippines
'Contact # : +63447977248(landline)
'Mobile # : +639224641939(SUN)
'+639757340073 (GLOBE)
'*****************************************************************************


#Include "fbgfx.bi"
#DEFINE __USE_GTK3__
#include once "gtk/gtk.bi"
#LIBPATH "../lib"
#Define NULL 0

#Include "fbcadcam-dec.bi"
#Include "fbcadcam-var.bi"
#include once "fbcadcam-macro.bas"
#include once "fbcadcam-gtk.bas"
#include once "fbcadcam_gcode.bas"


'The entry point of this program can be found near the bottom
'of fbcadcam-gtk.bas in this line of code:
'g_idle_add_full (G_PRIORITY_DEFAULT_IDLE, @mymain, NULL, NULL)

'What I mean by the entry point is:
'Once the GTK GUI (graphical user interface) is loaded completely
'and GTK is doing NOTHING it will enter into an IDLE state. And
'when this happens it will call a function called mymain.

'If you scroll down you can see the code in mymain.
'Upon review of this small function you should be able to see that
'fbcadcam is initialized (only once) and that the main loop for
'fbcadcam main loop is repeatedly called everytime GTK is IDLE.
'this main loop is the code that does 
'when the program is closed
End

Sub delete_group()
	Dim As Integer i,j,k
	Dim As BOOLEAN blockexploded(blockc),blockaffected(blockc)
	Dim As Integer entitiesdeletedc
	for i=1 to linec
		if lines(i,8)=1 Then
			entitiesdeletedc=entitiesdeletedc+1
			lines(i,8)=-1
			If lines(i,9)>0 Then
				blockaffected(lines(i,9))=TRUE
				blockstatus(lines(i,9))=-2'deleted
			EndIf
		EndIf
	Next
	For i=1 To circlec
		If circles(i,10)=1 Then
			entitiesdeletedc=entitiesdeletedc+1
			circles(i,10)=-1
			If circles(i,12)>0 Then
				blockaffected(circles(i,12))=TRUE
				blockstatus(circles(i,12))=-2'deleted
			EndIf
		EndIf
	Next
	theboxbelow("Entities deleted ="+Str(entitiesdeletedc)+crlf)
	For i = 1 To blockc
		If blockaffected(i)=TRUE Then'explode the block
			blockexploded(i)=TRUE
			For j=1 To linec
				If lines(j,9)=i Then
					'blockexploded(lines(j,9))=TRUE
					lines(j,9)=0
				EndIf
			Next
			For j=1 To circlec
				If circles(j,12)=i Then
					'blockexploded(circles(j,12))=TRUE
					circles(j,12)=0
				EndIf
			Next
		EndIf
	Next
	For i = 1 To blockc
		If blockexploded(i)=TRUE Then
			theboxbelow("Block "+blocknames(i)+" exploded"+crlf)
		Else
			If blockaffected(i)=TRUE Then theboxbelow("Block "+blocknames(i)+" deleted"+crlf)
		EndIf
	Next
	mousexp=mousex-1
	tempmousex=mousexp-1
	inview
	redraw
End Sub
Function disable_shortcut_keys Cdecl(byval widget as GtkWidget ptr, byval event as GdkEventKey ptr) as gboolean
	Dim As Integer i,j,k,keyvali
	If event->Type=GDK_KEY_RELEASE Then
		keyvali=event->keyval
		'mytextbuffer=Str(i)+Chr(10)+mytextbuffer
		'gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
		Select Case keyvali
			Case 65505,65506
				shift_key=0
			Case 65507,65508
				ctrl_key=0
			Case 65513,65514
				alt_key=0
			Case 65307
				escapeme
			Case 65535 'delete
				delete_group
			Case Asc("x")'use mygui
				If forcedx=TRUE Then
					forcex=FALSE
					forcedx=FALSE
				Else
					If drawing=FALSE Then
						setx1
					Else
						setx2
					EndIf
					'setxyz
				End If
				mousexp=mousex-1
				tempmousex=mousexp-1
			case Asc("X") 'no mygui - just set x1 or x2
				mousexp=mousex-1
				tempmousex=mousexp-1
				if forcedx=true Then
					forcex=FALSE
					forcedx=FALSE
				Else
					'If drawatangle=true Then
					'	altofxm
					'	createline
					'Else
						forcex=TRUE
						forcedx=TRUE
						If selentity=TRUE Then
							fx=fxm
						Else
							fx=damx
						EndIf
						fxd=fx
						If drawing=FALSE Then
							If forcey=TRUE Then
								x1=fx
								y1=fy
								drawmode=TRUE
								forcex=FALSE
								forcey=FALSE
								forcedx=FALSE
								forcedy=FALSE
							EndIf
						Else
							'If forcey=TRUE Then
							'	createline
							'End If
						End If
					'End If
				EndIf
			Case Asc("y")'use mygui
				If forcedy=TRUE Then
					forcey=FALSE
					forcedy=FALSE
				Else
					If drawing=FALSE Then
						sety1
					Else
						sety2
					EndIf
					'setxyz
				End If
				mousexp=mousex-1
				tempmousex=mousexp-1
			case Asc("Y")'no mygui just set y1 or y2
				mousexp=mousex-1
				tempmousex=mousexp-1
				if forcedy=true Then
					forcey=FALSE
					forcedy=FALSE
				Else
					'If drawatangle=true Then
					'	altofym
					'	createline
					'Else
						forcey=TRUE
						forcedy=TRUE
						If selentity=TRUE Then
							fy=fym
						Else
							fy=damy
						EndIf
						fyd=fy
						If drawing=FALSE Then
							If forcex=TRUE Then
								x1=fx
								y1=fy
								drawmode=TRUE
								forcex=FALSE
								forcey=FALSE
								forcedx=FALSE
								forcedy=FALSE
							EndIf
						Else
							'If forcex=TRUE Then
							'	createline
							'End If
						End If
					'End If
				EndIf
			'Case 5'"z"
			'Case 6'"Z"
			Case Asc("l")
				'get length from keybd or sellength
				If forcelength=TRUE Then
					forcelength=FALSE
					forcex=FALSE
					forcey=FALSE
				Else
					If drawing=TRUE Then
						newlength=Val(inputbox("double","Enter Length",Str(length)))
					EndIf
					if newlength>0 Then
						forcelength=TRUE
						forcedlength=newlength
					EndIf
				EndIf
			Case Asc("L")
				'get length from keybd or sellength
				If forcelength=TRUE Then
					forcelength=FALSE
					forcex=FALSE
					forcey=FALSE
				Else
					If drawing=TRUE Then
						Select Case selentity
							Case TRUE
								Select Case otd
									Case "line"
										newlength=Val(inputbox("double","Enter Length",Str(sellength)))
									Case "circle","arc"
										newlength=Val(inputbox("double","Enter Length",Str(circles(selcircle,4))))
								End Select
							Case FALSE
								newlength=Val(inputbox("double","Enter Length",Str(length)))
						End Select
					EndIf
					if newlength>0 Then
						forcelength=TRUE
						forcedlength=newlength
					EndIf
				EndIf
			Case Asc("o")
				'screenset 0,0:view:Window
				selbutton=21
				mousexp=mousex-1
				tempmousex=mousexp-1
				If ortho=TRUE Then
					turnbuttonoff
				Else
					turnbuttonon
				End If
			Case Asc("O")
				'screenset 0,0:view:Window
				mousexp=mousex-1
				tempmousex=mousexp-1
				If ortho=TRUE Then
					selbutton=21
					turnbuttonoff
				Else
					drawlineatangle2
					selbutton=21
					turnbuttonon
				End If
			Case Asc("a")
				'sometimes this should not be enabled
				'ie. if drawing lines and x1,y1 are not defined yet
				'If buttonson(1)=TRUE And drawing=TRUE Then drawlineatangle1
				drawlineatangle1
			Case Asc("A")
				'If buttonson(1)=TRUE And drawing=TRUE Then drawlineatangle2
				drawlineatangle2
			Case Asc("p")'construction projections (rays)
				If rayenabled=TRUE Then
					rayenabled=FALSE
					For i = rayi To rayi+7
						lines(i,8)=-1
					Next
					inview
					redraw
				Else
					If selentity=TRUE Then
						rayenabled=TRUE
						'set 4 orth lines
						'set 4 perp/para lines
						'with length wx2-wx1
						'fx=x1+cos(angle*d2r)*newlength
						'fy=y1+sin(angle*d2r)*newlength
						newlength=wx2-wx1
						Select Case otd
							Case "line"
								perpangle=selangle
							Case "circle","arc"
								xlength=fxm-circles(selcircle,1)
								ylength=fym-circles(selcircle,2)
								fixperpangle
							Case "ellipse","elliptical arc"
								perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
								If perpangle<0 then perpangle=360+perpangle
						End Select
						lines(rayi,1)=fxm
						lines(rayi,2)=fym
						lines(rayi,4)=fxm+cos(0*d2r)*newlength
						lines(rayi,5)=fym+sin(0*d2r)*newlength
						lines(rayi,7)=34
						lines(rayi,8)=0
						lines(rayi+1,1)=fxm
						lines(rayi+1,2)=fym
						lines(rayi+1,4)=fxm+cos(90*d2r)*newlength
						lines(rayi+1,5)=fym+sin(90*d2r)*newlength
						lines(rayi+1,7)=34
						lines(rayi+1,8)=0
						lines(rayi+2,1)=fxm
						lines(rayi+2,2)=fym
						lines(rayi+2,4)=fxm+cos(180*d2r)*newlength
						lines(rayi+2,5)=fym+sin(180*d2r)*newlength
						lines(rayi+2,7)=34
						lines(rayi+2,8)=0
						lines(rayi+3,1)=fxm
						lines(rayi+3,2)=fym
						lines(rayi+3,4)=fxm+cos(270*d2r)*newlength
						lines(rayi+3,5)=fym+sin(270*d2r)*newlength
						lines(rayi+3,7)=34
						lines(rayi+3,8)=0
						lines(rayi+4,1)=fxm
						lines(rayi+4,2)=fym
						lines(rayi+4,4)=fxm+cos((perpangle+0)*d2r)*newlength
						lines(rayi+4,5)=fym+sin((perpangle+0)*d2r)*newlength
						lines(rayi+4,7)=34
						lines(rayi+4,8)=0
						lines(rayi+5,1)=fxm
						lines(rayi+5,2)=fym
						lines(rayi+5,4)=fxm+cos((perpangle+90)*d2r)*newlength
						lines(rayi+5,5)=fym+sin((perpangle+90)*d2r)*newlength
						lines(rayi+5,7)=34
						lines(rayi+5,8)=0
						lines(rayi+6,1)=fxm
						lines(rayi+6,2)=fym
						lines(rayi+6,4)=fxm+cos((perpangle+180)*d2r)*newlength
						lines(rayi+6,5)=fym+sin((perpangle+180)*d2r)*newlength
						lines(rayi+6,7)=34
						lines(rayi+6,8)=0
						lines(rayi+7,1)=fxm
						lines(rayi+7,2)=fym
						lines(rayi+7,4)=fxm+cos((perpangle+270)*d2r)*newlength
						lines(rayi+7,5)=fym+sin((perpangle+270)*d2r)*newlength
						lines(rayi+7,7)=34
						lines(rayi+7,8)=0
						inview
						redraw
					EndIf
				EndIf
			Case Asc("P")'construction projections (rays)
				If rayenabled=TRUE Then
					rayenabled=FALSE
					For i = rayi To rayi+7
						lines(i,8)=-1
					Next
					inview
					redraw
				Else
					If selentity=TRUE Then
						rayenabled=TRUE
						'set 4 orth lines
						'set 4 perp/para lines
						'with length wx2-wx1
						'fx=x1+cos(angle*d2r)*newlength
						'fy=y1+sin(angle*d2r)*newlength
						newlength=Val(inputbox("double","Enter Length",Str((wx2-wx1))))
						Select Case otd
							Case "line"
								perpangle=selangle
							Case "circle","arc"
								xlength=fxm-circles(selcircle,1)
								ylength=fym-circles(selcircle,2)
								fixperpangle
							Case "ellipse","elliptical arc"
								perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
								If perpangle<0 then perpangle=360+perpangle
						End Select
						lines(rayi,1)=fxm
						lines(rayi,2)=fym
						lines(rayi,4)=fxm+cos(0*d2r)*newlength
						lines(rayi,5)=fym+sin(0*d2r)*newlength
						lines(rayi,7)=34
						lines(rayi,8)=0
						lines(rayi+1,1)=fxm
						lines(rayi+1,2)=fym
						lines(rayi+1,4)=fxm+cos(90*d2r)*newlength
						lines(rayi+1,5)=fym+sin(90*d2r)*newlength
						lines(rayi+1,7)=34
						lines(rayi+1,8)=0
						lines(rayi+2,1)=fxm
						lines(rayi+2,2)=fym
						lines(rayi+2,4)=fxm+cos(180*d2r)*newlength
						lines(rayi+2,5)=fym+sin(180*d2r)*newlength
						lines(rayi+2,7)=34
						lines(rayi+2,8)=0
						lines(rayi+3,1)=fxm
						lines(rayi+3,2)=fym
						lines(rayi+3,4)=fxm+cos(270*d2r)*newlength
						lines(rayi+3,5)=fym+sin(270*d2r)*newlength
						lines(rayi+3,7)=34
						lines(rayi+3,8)=0
						lines(rayi+4,1)=fxm
						lines(rayi+4,2)=fym
						lines(rayi+4,4)=fxm+cos((perpangle+0)*d2r)*newlength
						lines(rayi+4,5)=fym+sin((perpangle+0)*d2r)*newlength
						lines(rayi+4,7)=34
						lines(rayi+4,8)=0
						lines(rayi+5,1)=fxm
						lines(rayi+5,2)=fym
						lines(rayi+5,4)=fxm+cos((perpangle+90)*d2r)*newlength
						lines(rayi+5,5)=fym+sin((perpangle+90)*d2r)*newlength
						lines(rayi+5,7)=34
						lines(rayi+5,8)=0
						lines(rayi+6,1)=fxm
						lines(rayi+6,2)=fym
						lines(rayi+6,4)=fxm+cos((perpangle+180)*d2r)*newlength
						lines(rayi+6,5)=fym+sin((perpangle+180)*d2r)*newlength
						lines(rayi+6,7)=34
						lines(rayi+6,8)=0
						lines(rayi+7,1)=fxm
						lines(rayi+7,2)=fym
						lines(rayi+7,4)=fxm+cos((perpangle+270)*d2r)*newlength
						lines(rayi+7,5)=fym+sin((perpangle+270)*d2r)*newlength
						lines(rayi+7,7)=34
						lines(rayi+7,8)=0
						inview
						redraw
					EndIf
				EndIf
			Case Asc("g")
				degroup
				redraw
			case Asc("r")'set rotao (rotation angle offset)
				If modify=33 Or modify=34 Then
					rotao=Val(inputbox("double","Rotation angle offset",Str(angle)))
				End If
			case Asc("R")'set rotao (rotation angle offset)
				'what is the angle between pivot point and (mouse or snap point)
				If modify=33 Or modify=34 Then
					If selentity=TRUE Then
						xlength=fxm-modifyx1
						ylength=fym-modifyy1
						rotao=atan2(ylength,xlength)*r2d
						If rotao<0 then rotao=360+rotao
					Else
						xlength=damx-modifyx1
						ylength=damy-modifyy1
						rotao=atan2(ylength,xlength)*r2d
						If rotao<0 then rotao=360+rotao
					EndIf
				End If
			Case Asc("i"),Asc("I")
				If instructionaid=TRUE Then instructionaid=FALSE Else instructionaid=TRUE
				mousex=0
				mousey=0
				mousexp=1
				mouseyp=1
			Case Asc("s")
				If showentitytype=TRUE Then
					showentitytype=FALSE
				Else
					showentitytype=TRUE
				EndIf
			Case Asc("+")
				zoomin
			Case Asc("-")
				zoomout
			Case 65477'f8 - view extents
				zoomextents
			Case 65478'f9  - view previous
				If viewsset=FALSE And viewsc>0 Then
					showview
				Else
					If viewsi>1 Then
						viewsi=viewsi-1
						showview
					EndIf
				EndIf
			Case 65479'f10 - view next
				If viewsset=FALSE And viewsc>0 Then
					showview
				Else
					If viewsi<viewsc Then
						viewsi=viewsi+1
						showview
					EndIf
				EndIf
			Case 65480'f11 - view first
				If viewsset=FALSE And viewsc>0 Then
					viewsi=1
					showview
				Else
					If viewsc>0 And viewsi>1 Then
						viewsi=1
						showview
					EndIf
				EndIf
			Case 65481'f12 - view last
				If viewsset=FALSE And viewsc>0 Then
					viewsi=viewsc
					showview
				Else
					If viewsc>0 And viewsi<viewsc Then
						viewsi=viewsc
						showview
					EndIf
				EndIf
			Case 65470'f1 - help
				Shell "start http://fbcadcam.com/fbcadcam_users_guide.html"
		End Select
	EndIf
	Return TRUE
End Function
Function shortcut_keys Cdecl(byval widget as GtkWidget ptr, byval event as GdkEventKey ptr) as gboolean
	Dim As Integer i,j,k,keyvali
	If event->Type=GDK_KEY_PRESS Then
		keyvali=event->keyval
		'mytextbuffer=Str(keyvali)+Chr(10)+mytextbuffer
		'gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
		
		'left shift key 65505
		'right shift key 65506
		'left ctrl key 65507
		'ritght ctrly key 65508
		'left alt key 65513
		'right alt key 65514
		'escape key 65307
		'delete key 65535
		'F1 key 65470
		'F2 key 65471
		'F3 key 65472
		'F4 key 65473
		'F5 key 65474
		'F6 key 65475
		'F7 key 65476
		'F8 key 65477
		'F9 key 65478
		'F10 key 65479
		'F11 key 65480
		'F12 key 65481
		Select Case keyvali
			Case 65505
				shift_key=1
			Case 65506
				shift_key=2
			Case 65507
				ctrl_key=1
			Case 65508
				ctrl_key=2
			Case 65513
				alt_key=1
			Case 65514
				alt_key=2
			Case 65362 'up arrow
				'pan up
				viewsset=FALSE
				If shift_key=1 Or shift_key=2 Then
					tempint=int((wy2-wy1)/2)
				Else
					tempint=int((wy2-wy1)/50)
				EndIf
				wy1=wy1+tempint
				wy2=wy2+tempint
				inview()
				redraw
			Case 65364 'down arrow
				'pan down
				viewsset=FALSE
				If shift_key=1 Or shift_key=2 Then
					tempint=int((wy2-wy1)/2)
				Else
					tempint=int((wy2-wy1)/50)
				EndIf
				wy1=wy1-tempint
				wy2=wy2-tempint
				inview()
				redraw
			Case 65361 'left arrow
				'pan left
				viewsset=FALSE
				If shift_key=1 Or shift_key=2 Then
					tempint=int((wx2-wx1)/2)
				Else
					tempint=int((wx2-wx1)/50)
				EndIf
				wx1=wx1-tempint
				wx2=wx2-tempint
				inview()
				redraw
			Case 65363 'right arrow
				'pan right
				viewsset=FALSE
				If shift_key=1 Or shift_key=2 Then
					tempint=int((wx2-wx1)/2)
				Else
					tempint=int((wx2-wx1)/50)
				EndIf
				wx1=wx1+tempint
				wx2=wx2+tempint
				inview()
				redraw
		End Select
	EndIf
	Return TRUE
End Function
Function mouse_scroll_event(byval widget as GtkWidget ptr, byval event as GdkEventScroll ptr) as gboolean
	updatefbgfxgtkimage=TRUE
	mousemoved=TRUE
	'zoom out
	If event->direction=GDK_SCROLL_DOWN Then
		mygtkimage_mouse_scroll_up=TRUE
		mousew-=1
	EndIf
	'zoom in
	If event->direction=GDK_SCROLL_UP Then
		mygtkimage_mouse_scroll_down=TRUE
		mousew+=1
	EndIf
	Return 0
End Function
Function mouse_motion_notify_event Cdecl ( Byval widget As GtkWidget Ptr, Byval event As GdkEventMotion Ptr ) As Integer
	mousemoved=TRUE
	mygtkimage_mouse_x = event->x
	mygtkimage_mouse_y = event->y
	mygtkimage_mouse_y=579-mygtkimage_mouse_y 'this is used to change the mousey to increase from bottom up
'                     if we used the screen option in the window command such as
'                     Window Screen(w.x1,w.y1)-(w.x2,w.y2)
'                     then we wouldn't need to use (mousey=579-mousey)
	mygtkimage_mouse_x=wx1+(wx2-wx1)*(mygtkimage_mouse_x/(580))
	mygtkimage_mouse_y=wy1+(wy2-wy1)*(mygtkimage_mouse_y/(580))
	Return 0
End Function
Function mouse_enter_event Cdecl ( Byval widget As GtkWidget Ptr, Byval event As GdkEventMotion Ptr ) As Integer
	gtk_widget_grab_focus(ebox)
	Return 0
End Function
Function calcd(cd1 as Double,cd2 As Double,cd3 as Double,cd4 as Double,cd5 as Double,cd6 as Double) as double
	calcd = sqr((cd1-cd4)^2 + (cd2-cd5)^2 + (cd3-cd6)^2)
end function
Function mymod(n As Double,m As Integer) As Double
	Select Case n
		Case 0
			mymod = 0
		Case CDbl(m)
			mymod = CDbl(m)
		Case Else
			If InStr(Str(n),"e")<>0 Then
				mymod=0
			Else
				If InStr(Str(n),".")<>0 Then
					mymod=Val(Str(val(Mid(Str(n),1,InStr(Str(n),".")-1)) Mod m)+"."+Mid(Str(n),InStr(Str(n),".")+1))
				Else
					mymod = (n Mod m)
				EndIf
			End If
	End Select
End Function
Function abtp(abtpx1 as Double,abtpy1 as Double,abtpz1 as Double,abtpx2 as Double,abtpy2 as Double,abtpz2 as Double) as Double
	xlength=abtpx2-abtpx1
	ylength=abtpy2-abtpy1
	fixangle
	Return angle
End Function
Function calccmr(cmrt As Double, cmr1 As Double,cmr2 As Double,cmr3 As Double,cmr4 As Double) as Double
	Dim As Double cmra,cmrb,cmrc
	cmra=(2*cmr2)+(-cmr1+cmr3)*cmrt
   cmrb=(2*cmr1-5*cmr2+4*cmr3-cmr4)*cmrt^2
   cmrc=(-cmr1+3*cmr2-3*cmr3+cmr4)*cmrt^3
   calccmr=.5*(cmra+cmrb+cmrc)
End Function
Function mouse_button_press_events(ByVal widget as GtkWidget ptr, byval event as GdkEventButton ptr) as gboolean
	If event->Type=GDK_BUTTON_PRESS Then
		Select Case event->button
			Case 1'left button
				mousebuttondown=TRUE
				mouseb=1
				'theboxbelow("left mouse button pressed")
			Case 2'middle button
				mouseb=3
				'theboxbelow("middle mouse button pressed")
			Case 3'right button
				mouseb=2
				'theboxbelow("right mouse button pressed")
				'escapeme
		End Select
	EndIf
	Return 0
End Function
Function mouse_button_release_events(ByVal widget as GtkWidget ptr, byval event as GdkEventButton ptr) as gboolean
	If event->Type=GDK_BUTTON_RELEASE Then
		mouseb=0
		Select Case event->button
			Case 1'left button
				
			Case 2'middle button
				
			Case 3'right button
				
		End Select
	EndIf
	Return 0
End Function
Sub toolbar_button_new_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	escapeme
	escapeall
	newdrawing
	openeddrawingname=""
	zoomextents
	theboxbelow("New Drawing")
	gtk_window_set_title(GTK_WINDOW(win), My_GTK_APP_name + "   NEW DRAWING")

End Sub
Sub toolbar_button_open_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	Dim as GtkWidget ptr dialog
	gtk_init( 0, NULL )
	dialog = gtk_file_chooser_dialog_new( "Open File", _
														NULL, _
														GTK_FILE_CHOOSER_ACTION_OPEN, _
														GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
														GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, _
														NULL )
	If( gtk_dialog_run( GTK_DIALOG( dialog ) ) = GTK_RESPONSE_ACCEPT ) then
		Dim as zstring ptr filename
		filename = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( dialog ) )
		theboxbelow(*filename)
		If LCase(Right(*filename,4))=".dxf" Then
			escapeme
			escapeall
			newdrawing
			openeddrawingname=*filename
			theboxbelow("Opening "+openeddrawingname+crlf)
			importdxf
			countactiveobjects
			theboxbelow(openeddrawingname+" is opened. Active: Line count="+LTrim(Str(activelinesc))+" Circle count="+LTrim(Str(activecirclesc))+" Block count="+LTrim(Str(activeblocksc))+crlf)
			gtk_window_set_title(GTK_WINDOW(win), My_GTK_APP_name + "   " + *filename)
		Else
			theboxbelow("Error opening "+openeddrawingname+crlf)
		EndIf
	End if
	gtk_widget_destroy( GTK_WIDGET(dialog) )
End Sub
Sub toolbar_button_save_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	Dim As Integer i,j,k,c,templinec,tempcirclec
	If openeddrawingname="" Then
		Dim as GtkWidget ptr dialog
		gtk_init( 0, NULL )
		dialog = gtk_file_chooser_dialog_new( "Save File", _
															NULL, _
															GTK_FILE_CHOOSER_ACTION_SAVE, _
															GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
															GTK_STOCK_SAVE, GTK_RESPONSE_ACCEPT, _
															NULL )
		If( gtk_dialog_run( GTK_DIALOG( dialog ) ) = GTK_RESPONSE_ACCEPT ) then
			Dim as zstring ptr filename
			filename = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( dialog ) )
			theboxbelow(*filename)
			tempstring=*filename
			If LCase(Right(tempstring,4))<>".dxf" Then tempstring+=".dxf"
			theboxbelow(tempstring)
			gtk_window_set_title(GTK_WINDOW(win), My_GTK_APP_name + "   " + tempstring)
			openeddrawingname=tempstring
			savedxf(tempstring)
			theboxbelow("Drawing saved as "+openeddrawingname+crlf)
		End if
		gtk_widget_destroy( GTK_WIDGET(dialog) )
	Else
		savedxf(openeddrawingname)
		theboxbelow("Drawing saved as "+openeddrawingname+crlf)
	End If
	If edit_master_block=TRUE Then
		theboxbelow("edited block saved - next step - updating drawing with edited block")
		'enable block buttons and turn edit_master_block off
		disable_all_block_buttons=FALSE
		're-open the temp file
		escapeme
		escapeall
		newdrawing
		openeddrawingname=edit_master_block_from_dxf_file_name
		importdxf
		countactiveobjects
		gtk_window_set_title(GTK_WINDOW(win), My_GTK_APP_name + "   " + openeddrawingname)
		buttonson(113)=FALSE
		blocknames(edit_master_block_number)=blocknames(edit_master_block_number)+"_old"
		For k = 1 To edit_blocks_inserts_c
			'rename blocknames() of all refs to block to block name _ old including the master block
			blocknames(edit_blocks_inserts(k))=blocknames(edit_blocks_inserts(k))+"_old"
			'import modified master block into drawing at:
			'@ insertion points of edit_blocks_inserts()
			'@ rotation and scale of edit_blocks_inserts()
			blockname=edit_master_block_name
			'If buttonson(113)=TRUE Then
			'	blockoffsets(blockc,1)=insertionx
			'	blockoffsets(blockc,2)=insertiony
			'insertionx=0
			'insertiony=0
			'buttonson(113)=TRUE
			If k=1 Then
				importblock
			Else
				copybaseblock(blockc-k+1)
			EndIf
			i=blockc-k
			modifyx1=0
			modifyy1=0
			modifyx2=blockoffsets(edit_blocks_inserts(k),1)
			modifyy2=blockoffsets(edit_blocks_inserts(k),2)
			theboxbelow("modify:"+Str(modifyx1)+","+Str(modifyy1)+","+Str(modifyx2)+","+Str(modifyy2))
			'movedxfblock(blockc)
			setmovedown
			'this is the rotation and it was working
			'prior to working on scale factors
			If blockoffsets(edit_blocks_inserts(k),7)<>0 Then
				If blockoffsets(edit_blocks_inserts(k),7)<>360 Then
					angle=blockoffsets(edit_blocks_inserts(k),7)
					modifyx1=blockoffsets(blockc,1)
					modifyy1=blockoffsets(blockc,2)
					rotateblock(blockc)
				EndIf
			EndIf
			blockoffsets(blockc,4)=blockoffsets(edit_blocks_inserts(k),4)
			blockoffsets(blockc,5)=blockoffsets(edit_blocks_inserts(k),5)
			blockoffsets(blockc,6)=blockoffsets(edit_blocks_inserts(k),6)
			If blockoffsets(blockc,4)=0 Then blockoffsets(blockc,4)=1
			If blockoffsets(blockc,5)=0 Then blockoffsets(blockc,5)=1
			If blockoffsets(blockc,6)=0 Then blockoffsets(blockc,6)=1
			If blockoffsets(blockc,4)=1 And blockoffsets(blockc,5)=1 Then
				'do nothing
			Else
				'scale the lines
				'remember to mod this if negative scaling exists
				templinec=linec+1
				For j=1 To linec
					If lines(j,9)=blockc Then
						templinec=j
						Exit For
					EndIf
				Next
				tempcirclec=linec+1
				For j=1 To circlec
					If circles(j,12)=blockc Then
						tempcirclec=j
						Exit For
					EndIf
				Next
				
				For j = templinec To linec
					lines(j,1)=blockoffsets(blockc,1)+(lines(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
					lines(j,4)=blockoffsets(blockc,1)+(lines(j,4)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
					lines(j,2)=blockoffsets(blockc,2)+(lines(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
					lines(j,5)=blockoffsets(blockc,2)+(lines(j,5)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
				Next
				'inview()
				'redraw
				'now, for circles there are a lot of things to do
				If blockoffsets(blockc,4)=blockoffsets(blockc,5) Then
					'scale x,y,z all the same
					For j = tempcirclec To circlec
						circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
						circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
						circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
						circles(j,8)=circles(j,8)*blockoffsets(blockc,4)
					Next
				Else
					For j = tempcirclec To circlec
						Select Case circles(j,9)
							Case 1
								'just for circles or arcs (originally)
								'turn them into ellipses here
								'this is not done yet
								circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
								circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
								circles(j,8)=circles(j,4)
								circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
								circles(j,8)=circles(j,8)*blockoffsets(blockc,5)
								circles(j,6)=0
								circles(j,7)=360
								circles(j,9)=3
							Case 2
								circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
								circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
								circles(j,8)=circles(j,4)
								circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
								circles(j,8)=circles(j,8)*blockoffsets(blockc,5)
								circles(j,6)=circles(j,6)*180/pi
								circles(j,7)=circles(j,7)*180/pi
								circles(j,9)=4
							Case 3,4
								'set selcircle to j
								'and pass i to monkeysmatter
								'make sure this works for elliptical arcs
								Select Case circles(j,11)
									Case 0,180,360
										circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
										circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
										circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
										circles(j,8)=circles(j,8)*blockoffsets(blockc,5)
									Case 90,270
										circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
										circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
										circles(j,4)=circles(j,4)*blockoffsets(blockc,5)
										circles(j,8)=circles(j,8)*blockoffsets(blockc,4)
									Case Else
										selcircle=j
										monkeysmatter(i,blockc)
								End Select
						End Select
					Next
				EndIf
			EndIf
			for i=1 to linec
				If lines(i,9)=edit_blocks_inserts(k) Then lines(i,8)=1
			Next
			For i=1 To circlec
				If circles(i,12)=edit_blocks_inserts(k) Then circles(i,10)=1
			Next
			delete_group
		Next
		theboxbelow("block updated")
		selbutton=116
		buttonmanager
		redraw
		showview
		edit_master_block=FALSE
	EndIf
	
End Sub
Sub toolbar_button_save_as_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	Dim as GtkWidget ptr dialog
	gtk_init( 0, NULL )
	dialog = gtk_file_chooser_dialog_new( "Save File AS", _
														NULL, _
														GTK_FILE_CHOOSER_ACTION_SAVE, _
														GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
														GTK_STOCK_SAVE_AS, GTK_RESPONSE_ACCEPT, _
														NULL )
	If( gtk_dialog_run( GTK_DIALOG( dialog ) ) = GTK_RESPONSE_ACCEPT ) then
		Dim as zstring ptr filename
		filename = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( dialog ) )
		theboxbelow(*filename)
		tempstring=*filename
		If LCase(Right(tempstring,4))<>".dxf" Then tempstring+=".dxf"
		theboxbelow(tempstring)
		gtk_window_set_title(GTK_WINDOW(win), My_GTK_APP_name + "   " + tempstring)
		openeddrawingname=tempstring
		savedxf(tempstring)
		theboxbelow("Drawing saved as "+openeddrawingname+crlf)
	End if
	gtk_widget_destroy( GTK_WIDGET(dialog) )
End Sub
Sub toolbar_button_cut_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	
End Sub
Sub toolbar_button_copy_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	
End Sub
Sub toolbar_button_paste_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	
End Sub
Sub toolbar_button_delete_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	delete_group
End Sub
Sub toolbar_button_undo_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	
End Sub
Sub toolbar_button_redo_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	
End Sub
Sub toolbar_button_zoom_in_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	zoomin
End Sub
Sub toolbar_button_zoom_out_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	zoomout
End Sub
Sub toolbar_button_zoom_fit_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	zoomextents
End Sub
Sub toolbar_button_zoom_100_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	viewsset=FALSE
	mousexp=mousex-1
	tempmousex=mousexp-1
	drawareax1=0
	drawareax2=579
	drawareay1=0
	drawareay2=579
	wx1=0
	wy1=0
	wx2=drawareax2-drawareax1
	wy2=drawareay2-drawareay1
	wzoom=(wx2-wx1)/wzoomt
	theboxbelow("Zoom Default")
	inview()
	redraw
End Sub
Sub toolbar_button_macro_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	Dim As GtkWidget PTR dialog,content
	Dim As GtkWidget PTR dg'dialog grid
	'Dim As GtkWidget PTR dgl1,dgl2,dgl3'dialog grid label(n)
	'Dim As GtkWidget PTR dge1,dge2,dge3'dialog grid entry(n)
	
	Dim As GtkWidget Ptr dgtextarea
	Dim As GtkTextIter dgtextbufstart, dgtextbufend
	Dim As GtkTextMark Ptr dgtextbufmark
	Dim As GtkTextBuffer Ptr dgbuffer
	Dim As GtkWidget Ptr dgtasw
	Dim As String macro_string
	Dim As ZString Ptr macro_zstring
	Dim as ZString ptr filename
	Dim As String macrofilename
	macrofilename="(Untitled).bas"
	
	dgtextarea = gtk_text_view_new()
	dgbuffer = gtk_text_view_get_buffer (GTK_TEXT_VIEW (dgtextarea))
	dgtasw = gtk_scrolled_window_new(NULL, NULL)
	gtk_widget_set_size_request(dgtasw, 800, 400)
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW (dgtasw),GTK_POLICY_AUTOMATIC, GTK_POLICY_ALWAYS)
	gtk_container_add(GTK_SCROLLED_WINDOW (dgtasw),dgtextarea)

	Dim As Integer response
	dialog = gtk_dialog_new()
	gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
	gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
	gtk_window_set_title(GTK_WINDOW(dialog), "FBCADCAM MACRO (Untitled).bas")
	gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
	'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 100)
	content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
	
	dg = gtk_grid_new()
	'dgl1 = gtk_label_new ("")
	'dgl2 = gtk_label_new ("")
	'dgl3 = gtk_label_new ("")
	'dge1 = gtk_entry_new()
	'dge2 = gtk_entry_new()
	'dge3 = gtk_entry_new()
	'gtk_entry_set_width_chars (dge1,2)
	'gtk_entry_set_width_chars (dge2,10)
	'gtk_entry_set_width_chars (dge3,10)
	'gtk_entry_set_text(GTK_ENTRY(dge1),"")
	'gtk_entry_set_text(GTK_ENTRY(dge2),"")
	'gtk_entry_set_text(GTK_ENTRY(dge3),"")
	'gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
	'gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
	'gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
	'gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
	'gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
	'gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
	
	gtk_grid_attach (GTK_GRID(dg), dgtasw, 0, 0, 1, 1)
	
	gtk_container_add (GTK_CONTAINER(content), dg)
	gtk_dialog_add_button(GTK_DIALOG(dialog), "NEW", GTK_RESPONSE_OK)
	gtk_dialog_add_button(GTK_DIALOG(dialog), "OPEN", GTK_RESPONSE_YES)
	gtk_dialog_add_button(GTK_DIALOG(dialog), "SAVE", GTK_RESPONSE_ACCEPT)
	gtk_dialog_add_button(GTK_DIALOG(dialog), "RUN", GTK_RESPONSE_APPLY)
	
	'GTK_RESPONSE_NONE = -1
	'GTK_RESPONSE_REJECT = -2
	'GTK_RESPONSE_ACCEPT = -3
	'GTK_RESPONSE_DELETE_EVENT = -4
	'GTK_RESPONSE_OK = -5
	'GTK_RESPONSE_CANCEL = -6
	'GTK_RESPONSE_CLOSE = -7
	'GTK_RESPONSE_YES = -8
	'GTK_RESPONSE_NO = -9
	'GTK_RESPONSE_APPLY = -10
	'GTK_RESPONSE_HELP = -11	
	
	'gtk_widget_grab_focus (page5)
	gtk_text_buffer_set_text (dgbuffer, last_macro_string, -1)
	
	gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
	gtk_widget_show_all (dialog)
	Do
		Sleep 1
		response = gtk_dialog_run(GTK_DIALOG(dialog))
		Select Case response
			Case -5
				'new
				macro_string=""
				last_macro_string=macro_string
				gtk_window_set_title(GTK_WINDOW(dialog), "FBCADCAM MACRO (Untitled).bas")
				gtk_text_buffer_set_text (dgbuffer, "", -1)
			Case -8
				'open
				Dim as GtkWidget ptr macro_open_dialog
				gtk_init( 0, NULL )
				macro_open_dialog = gtk_file_chooser_dialog_new( "Open File", _
																	NULL, _
																	GTK_FILE_CHOOSER_ACTION_OPEN, _
																	GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
																	GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, _
																	NULL )
				If( gtk_dialog_run( GTK_DIALOG( macro_open_dialog ) ) = GTK_RESPONSE_ACCEPT ) then
					filename = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( macro_open_dialog ) )
					macrofilename = *filename
					gtk_window_set_title(GTK_WINDOW(dialog), "FBCADCAM MACRO " + *filename)
					macro_string=""
					ff=FreeFile
					Open macrofilename For Append As #ff
					Close #ff
					Open macrofilename For Input As #ff
					Do While Not Eof(ff)
						Line Input #ff, tempstring
						macro_string+=tempstring+Chr(10)
					Loop
					Close #ff
					last_macro_string=macro_string
					gtk_text_buffer_set_text (dgbuffer, macro_string, -1)
				End if
				gtk_widget_destroy( GTK_WIDGET(macro_open_dialog) )
			Case -3
				'save
				Dim as GtkWidget ptr macro_save_dialog
				Dim as GtkFileChooser Ptr chooser
				gtk_init( 0, NULL )
				macro_save_dialog = gtk_file_chooser_dialog_new( "Save File AS", _
																	NULL, _
																	GTK_FILE_CHOOSER_ACTION_SAVE, _
																	GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
																	GTK_STOCK_SAVE_AS, GTK_RESPONSE_ACCEPT, _
																	NULL )
				chooser = GTK_FILE_CHOOSER (macro_save_dialog)
				'gtk_file_chooser_set_do_overwrite_confirmation (chooser, TRUE)
				gtk_file_chooser_set_filename (chooser,*filename)
				If( gtk_dialog_run( GTK_DIALOG( macro_save_dialog ) ) = GTK_RESPONSE_ACCEPT ) then
					filename = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( macro_save_dialog ) )
					macrofilename = *filename
					gtk_window_set_title(GTK_WINDOW(dialog), "FBCADCAM MACRO " + macrofilename)
					gtk_text_buffer_get_start_iter (dgbuffer, @dgtextbufstart)
					gtk_text_buffer_get_end_iter (dgbuffer, @dgtextbufend)
					macro_zstring = gtk_text_buffer_get_text(dgbuffer,@dgtextbufstart,@dgtextbufend,FALSE)
					Open macrofilename For Output As #ff
					Print #ff,*macro_zstring
					Close #ff
					last_macro_string=*macro_zstring
				End if
				gtk_widget_destroy( GTK_WIDGET(macro_save_dialog) )
			Case -10
				'run
				If macrofilename="(Untitled).bas" Then
					'save the contents to "(Untitled).bas"
					gtk_text_buffer_get_start_iter (dgbuffer, @dgtextbufstart)
					gtk_text_buffer_get_end_iter (dgbuffer, @dgtextbufend)
					macro_zstring = gtk_text_buffer_get_text(dgbuffer,@dgtextbufstart,@dgtextbufend,FALSE)
					last_macro_string=*macro_zstring
					Open macrofilename For Output As #ff
					Print #ff,*macro_zstring
					Close #ff
				Else
					Open macrofilename For Append As #ff
					Close #ff
				EndIf
				theboxbelow("running macro " + macrofilename)
				import_macro_bas_file(macrofilename)
				theboxbelow(fbcadcam_macro_debug)
				inview
				redraw
				gtk_widget_destroy(dialog)
				Exit Do
			Case Else
				gtk_widget_destroy(dialog)
				Exit Do
		End Select
	Loop
End Sub
Sub toolbar_button_help_op Cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
	Shell "start http://fbcadcam.com/fbcadcam_users_guide.html"
End Sub
Sub baeg_swap Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			g_object_ref(bae1gb1)
			g_object_ref(bae1gb2)
			g_object_ref(bae1gb3)
			g_object_ref(bae1gb4)
			g_object_ref(bae1gb5)
			g_object_ref(bae1gb6)
			g_object_ref(bae1gl1)
			g_object_ref(bae1gl2)
			g_object_ref(bae1gl3)
			g_object_ref(bae1gl4)
			g_object_ref(bae1gl5)
			g_object_ref(bae1gl6)
			If bae1gab=TRUE Then
				bae1gab=FALSE
				gtk_button_set_label(bae1gridab, "+")
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb1)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb2)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb3)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb4)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb5)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb6)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gl1)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gl2)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gl3)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gl4)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gl5)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gl6)
				gtk_grid_set_column_spacing (GTK_GRID(bae1grida),0)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb6, 1, 1, 1, 1)
				gtk_widget_set_tooltip_text (bae1gb1,"Draw Lines")
				gtk_widget_set_tooltip_text (bae1gb2,"Draw Circles")
				gtk_widget_set_tooltip_text (bae1gb3,"Draw Arcs")
				gtk_widget_set_tooltip_text (bae1gb4,"Draw Ellipses")
				gtk_widget_set_tooltip_text (bae1gb5,"Draw Elliptical Arcs")
				gtk_widget_set_tooltip_text (bae1gb6,"Draw Splines")
			Else
				bae1gab=TRUE
				gtk_button_set_label(bae1gridab, "-")
				gtk_widget_set_has_tooltip(bae1gb1,FALSE)
				gtk_widget_set_has_tooltip(bae1gb2,FALSE)
				gtk_widget_set_has_tooltip(bae1gb3,FALSE)
				gtk_widget_set_has_tooltip(bae1gb4,FALSE)
				gtk_widget_set_has_tooltip(bae1gb5,FALSE)
				gtk_widget_set_has_tooltip(bae1gb6,FALSE)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb1)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb2)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb3)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb4)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb5)
				gtk_container_remove (GTK_GRID(bae1grida), bae1gb6)
				gtk_grid_set_column_spacing (GTK_GRID(bae1grida),10)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae1grida), bae1gb6, 0, 5, 1, 1)
				gtk_widget_set_halign (bae1gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae1gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae1gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae1gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae1gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae1gl6, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae1grida), bae1gl1, bae1gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae1grida), bae1gl2, bae1gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae1grida), bae1gl3, bae1gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae1grida), bae1gl4, bae1gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae1grida), bae1gl5, bae1gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae1grida), bae1gl6, bae1gb6, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 2
			g_object_ref(bae2gb1)
			g_object_ref(bae2gb2)
			g_object_ref(bae2gb3)
			g_object_ref(bae2gb4)
			g_object_ref(bae2gb5)
			g_object_ref(bae2gb6)
			g_object_ref(bae2gb7)
			g_object_ref(bae2gb8)
			g_object_ref(bae2gb9)
			g_object_ref(bae2gb10)
			g_object_ref(bae2gl1)
			g_object_ref(bae2gl2)
			g_object_ref(bae2gl3)
			g_object_ref(bae2gl4)
			g_object_ref(bae2gl5)
			g_object_ref(bae2gl6)
			g_object_ref(bae2gl7)
			g_object_ref(bae2gl8)
			g_object_ref(bae2gl9)
			g_object_ref(bae2gl10)
			If bae2gab=TRUE Then
				bae2gab=FALSE
				gtk_button_set_label(bae2gridab, "+")
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb1)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb2)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb3)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb4)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb5)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb6)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb7)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb8)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb9)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb10)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl1)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl2)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl3)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl4)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl5)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl6)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl7)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl8)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl9)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gl10)
				gtk_grid_set_column_spacing (GTK_GRID(bae2grida),0)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb8, 3, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb9, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb10, 1, 2, 1, 1)
				gtk_widget_set_tooltip_text (bae2gb1,"Draw Equilateral Triangles")
				gtk_widget_set_tooltip_text (bae2gb2,"Draw Right Triangles")
				gtk_widget_set_tooltip_text (bae2gb3,"Draw Squares")
				gtk_widget_set_tooltip_text (bae2gb4,"Draw Rectangles")
				gtk_widget_set_tooltip_text (bae2gb5,"Draw Rhombus")
				gtk_widget_set_tooltip_text (bae2gb6,"Draw Parallelograms")
				gtk_widget_set_tooltip_text (bae2gb7,"Draw Pentagons")
				gtk_widget_set_tooltip_text (bae2gb8,"Draw Hexagons")
				gtk_widget_set_tooltip_text (bae2gb9,"Draw Heptagons")
				gtk_widget_set_tooltip_text (bae2gb10,"Draw Octagons")
			Else
				bae2gab=TRUE
				gtk_button_set_label(bae2gridab, "-")
				gtk_widget_set_has_tooltip(bae2gb1,FALSE)
				gtk_widget_set_has_tooltip(bae2gb2,FALSE)
				gtk_widget_set_has_tooltip(bae2gb3,FALSE)
				gtk_widget_set_has_tooltip(bae2gb4,FALSE)
				gtk_widget_set_has_tooltip(bae2gb5,FALSE)
				gtk_widget_set_has_tooltip(bae2gb6,FALSE)
				gtk_widget_set_has_tooltip(bae2gb7,FALSE)
				gtk_widget_set_has_tooltip(bae2gb8,FALSE)
				gtk_widget_set_has_tooltip(bae2gb9,FALSE)
				gtk_widget_set_has_tooltip(bae2gb10,FALSE)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb1)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb2)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb3)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb4)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb5)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb6)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb7)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb8)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb9)
				gtk_container_remove (GTK_GRID(bae2grida), bae2gb10)
				gtk_grid_set_column_spacing (GTK_GRID(bae2grida),10)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb8, 0, 7, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb9, 0, 8, 1, 1)
				gtk_grid_attach (GTK_GRID(bae2grida), bae2gb10, 0, 9, 1, 1)
				gtk_widget_set_halign (bae2gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl8, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl9, GTK_ALIGN_START)
				gtk_widget_set_halign (bae2gl10, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl1, bae2gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl2, bae2gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl3, bae2gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl4, bae2gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl5, bae2gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl6, bae2gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl7, bae2gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl8, bae2gb8, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl9, bae2gb9, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae2grida), bae2gl10, bae2gb10, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 3
			g_object_ref(bae3gb1)
			g_object_ref(bae3gb2)
			g_object_ref(bae3gb3)
			g_object_ref(bae3gb4)
			g_object_ref(bae3gb5)
			g_object_ref(bae3gb6)
			g_object_ref(bae3gb7)
			g_object_ref(bae3gb8)
			g_object_ref(bae3gl1)
			g_object_ref(bae3gl2)
			g_object_ref(bae3gl3)
			g_object_ref(bae3gl4)
			g_object_ref(bae3gl5)
			g_object_ref(bae3gl6)
			g_object_ref(bae3gl7)
			g_object_ref(bae3gl8)
			If bae3gab=TRUE Then
				bae3gab=FALSE
				gtk_button_set_label(bae3gridab, "+")
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb1)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb2)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb3)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb4)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb5)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb6)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb7)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb8)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl1)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl2)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl3)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl4)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl5)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl6)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl7)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gl8)
				gtk_grid_set_column_spacing (GTK_GRID(bae3grida),0)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb8, 3, 1, 1, 1)
				gtk_widget_set_tooltip_text (bae3gb1,"Snap to End point")
				gtk_widget_set_tooltip_text (bae3gb2,"Snap to Mid point")
				gtk_widget_set_tooltip_text (bae3gb3,"Snap to Perpendicular")
				gtk_widget_set_tooltip_text (bae3gb4,"Snap to Tangent")
				gtk_widget_set_tooltip_text (bae3gb5,"Snap to Arc Center")
				gtk_widget_set_tooltip_text (bae3gb6,"Snap to Intersection")
				gtk_widget_set_tooltip_text (bae3gb7,"Snap to Nearest point")
				gtk_widget_set_tooltip_text (bae3gb8,"Snap to Arc Intercept")
			Else
				bae3gab=TRUE
				gtk_button_set_label(bae3gridab, "-")
				gtk_widget_set_has_tooltip(bae3gb1,FALSE)
				gtk_widget_set_has_tooltip(bae3gb2,FALSE)
				gtk_widget_set_has_tooltip(bae3gb3,FALSE)
				gtk_widget_set_has_tooltip(bae3gb4,FALSE)
				gtk_widget_set_has_tooltip(bae3gb5,FALSE)
				gtk_widget_set_has_tooltip(bae3gb6,FALSE)
				gtk_widget_set_has_tooltip(bae3gb7,FALSE)
				gtk_widget_set_has_tooltip(bae3gb8,FALSE)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb1)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb2)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb3)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb4)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb5)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb6)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb7)
				gtk_container_remove (GTK_GRID(bae3grida), bae3gb8)
				gtk_grid_set_column_spacing (GTK_GRID(bae3grida),10)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae3grida), bae3gb8, 0, 7, 1, 1)
				gtk_widget_set_halign (bae3gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae3gl8, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl1, bae3gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl2, bae3gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl3, bae3gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl4, bae3gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl5, bae3gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl6, bae3gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl7, bae3gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae3grida), bae3gl8, bae3gb8, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 4
			g_object_ref(bae4gb1)
			g_object_ref(bae4gb2)
			g_object_ref(bae4gb3)
			g_object_ref(bae4gb4)
			g_object_ref(bae4gb5)
			g_object_ref(bae4gb6)
			g_object_ref(bae4gb7)
			g_object_ref(bae4gb8)
			g_object_ref(bae4gb9)
			g_object_ref(bae4gb10)
			g_object_ref(bae4gl1)
			g_object_ref(bae4gl2)
			g_object_ref(bae4gl3)
			g_object_ref(bae4gl4)
			g_object_ref(bae4gl5)
			g_object_ref(bae4gl6)
			g_object_ref(bae4gl7)
			g_object_ref(bae4gl8)
			g_object_ref(bae4gl9)
			g_object_ref(bae4gl10)
			If bae4gab=TRUE Then
				bae4gab=FALSE
				gtk_button_set_label(bae4gridab, "+")
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb1)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb2)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb3)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb4)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb5)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb6)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb7)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb8)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb9)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb10)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl1)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl2)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl3)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl4)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl5)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl6)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl7)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl8)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl9)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gl10)
				gtk_grid_set_column_spacing (GTK_GRID(bae4grida),0)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb8, 3, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb9, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb10, 1, 2, 1, 1)
				gtk_widget_set_tooltip_text (bae4gb1,"Orth mode")
				gtk_widget_set_tooltip_text (bae4gb2,"Use Angle")
				gtk_widget_set_tooltip_text (bae4gb3,"Set Preset Angles")
				gtk_widget_set_tooltip_text (bae4gb4,"Preset Angle(1) = " + Str(preset_angle_1))
				gtk_widget_set_tooltip_text (bae4gb5,"Preset Angle(2) = " + Str(preset_angle_2))
				gtk_widget_set_tooltip_text (bae4gb6,"Preset Angle(3) = " + Str(preset_angle_3))
				gtk_widget_set_tooltip_text (bae4gb7,"Preset Angle(4) = " + Str(preset_angle_4))
				gtk_widget_set_tooltip_text (bae4gb8,"Preset Angle(5) = " + Str(preset_angle_5))
				gtk_widget_set_tooltip_text (bae4gb9,"Perpendicular From")
				gtk_widget_set_tooltip_text (bae4gb10,"Tangent From")
			Else
				bae4gab=TRUE
				gtk_button_set_label(bae4gridab, "-")
				gtk_widget_set_has_tooltip(bae4gb1,FALSE)
				gtk_widget_set_has_tooltip(bae4gb2,FALSE)
				gtk_widget_set_has_tooltip(bae4gb3,FALSE)
				gtk_widget_set_has_tooltip(bae4gb4,FALSE)
				gtk_widget_set_has_tooltip(bae4gb5,FALSE)
				gtk_widget_set_has_tooltip(bae4gb6,FALSE)
				gtk_widget_set_has_tooltip(bae4gb7,FALSE)
				gtk_widget_set_has_tooltip(bae4gb8,FALSE)
				gtk_widget_set_has_tooltip(bae4gb9,FALSE)
				gtk_widget_set_has_tooltip(bae4gb10,FALSE)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb1)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb2)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb3)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb4)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb5)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb6)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb7)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb8)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb9)
				gtk_container_remove (GTK_GRID(bae4grida), bae4gb10)
				gtk_grid_set_column_spacing (GTK_GRID(bae4grida),10)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb8, 0, 7, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb9, 0, 8, 1, 1)
				gtk_grid_attach (GTK_GRID(bae4grida), bae4gb10, 0, 9, 1, 1)
				gtk_widget_set_halign (bae4gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl8, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl9, GTK_ALIGN_START)
				gtk_widget_set_halign (bae4gl10, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl1, bae4gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl2, bae4gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl3, bae4gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl4, bae4gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl5, bae4gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl6, bae4gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl7, bae4gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl8, bae4gb8, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl9, bae4gb9, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae4grida), bae4gl10, bae4gb10, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 5
			g_object_ref(bae5gb1)
			g_object_ref(bae5gb2)
			g_object_ref(bae5gb3)
			g_object_ref(bae5gb4)
			g_object_ref(bae5gb5)
			g_object_ref(bae5gb6)
			g_object_ref(bae5gb7)
			g_object_ref(bae5gb8)
			g_object_ref(bae5gb9)
			g_object_ref(bae5gb10)
			g_object_ref(bae5gl1)
			g_object_ref(bae5gl2)
			g_object_ref(bae5gl3)
			g_object_ref(bae5gl4)
			g_object_ref(bae5gl5)
			g_object_ref(bae5gl6)
			g_object_ref(bae5gl7)
			g_object_ref(bae5gl8)
			g_object_ref(bae5gl9)
			g_object_ref(bae5gl10)
			If bae5gab=TRUE Then
				bae5gab=FALSE
				gtk_button_set_label(bae5gridab, "+")
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb1)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb2)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb3)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb4)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb5)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb6)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb7)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb8)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb9)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb10)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl1)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl2)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl3)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl4)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl5)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl6)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl7)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl8)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl9)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gl10)
				gtk_grid_set_column_spacing (GTK_GRID(bae5grida),0)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb8, 3, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb9, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb10, 1, 2, 1, 1)
				gtk_widget_set_tooltip_text (bae5gb1,"Move")
				gtk_widget_set_tooltip_text (bae5gb2,"Copy")
				gtk_widget_set_tooltip_text (bae5gb3,"Rotate")
				gtk_widget_set_tooltip_text (bae5gb4,"Copy Rotate")
				gtk_widget_set_tooltip_text (bae5gb5,"Flip Vertical")
				gtk_widget_set_tooltip_text (bae5gb6,"Flip Horizontal")
				gtk_widget_set_tooltip_text (bae5gb7,"Copy Flip Vertical")
				gtk_widget_set_tooltip_text (bae5gb8,"Copy Flip Horizontal")
				gtk_widget_set_tooltip_text (bae5gb9,"Flip Vertical & Horizontal")
				gtk_widget_set_tooltip_text (bae5gb10,"Copy Flip Vertical & Horizontal")
			Else
				bae5gab=TRUE
				gtk_button_set_label(bae5gridab, "-")
				gtk_widget_set_has_tooltip(bae5gb1,FALSE)
				gtk_widget_set_has_tooltip(bae5gb2,FALSE)
				gtk_widget_set_has_tooltip(bae5gb3,FALSE)
				gtk_widget_set_has_tooltip(bae5gb4,FALSE)
				gtk_widget_set_has_tooltip(bae5gb5,FALSE)
				gtk_widget_set_has_tooltip(bae5gb6,FALSE)
				gtk_widget_set_has_tooltip(bae5gb7,FALSE)
				gtk_widget_set_has_tooltip(bae5gb8,FALSE)
				gtk_widget_set_has_tooltip(bae5gb9,FALSE)
				gtk_widget_set_has_tooltip(bae5gb10,FALSE)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb1)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb2)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb3)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb4)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb5)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb6)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb7)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb8)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb9)
				gtk_container_remove (GTK_GRID(bae5grida), bae5gb10)
				gtk_grid_set_column_spacing (GTK_GRID(bae5grida),10)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb8, 0, 7, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb9, 0, 8, 1, 1)
				gtk_grid_attach (GTK_GRID(bae5grida), bae5gb10, 0, 9, 1, 1)
				gtk_widget_set_halign (bae5gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl8, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl9, GTK_ALIGN_START)
				gtk_widget_set_halign (bae5gl10, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl1, bae5gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl2, bae5gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl3, bae5gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl4, bae5gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl5, bae5gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl6, bae5gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl7, bae5gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl8, bae5gb8, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl9, bae5gb9, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae5grida), bae5gl10, bae5gb10, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 6
			g_object_ref(bae6gb1)
			g_object_ref(bae6gb2)
			g_object_ref(bae6gb3)
			g_object_ref(bae6gb4)
			g_object_ref(bae6gb5)
			g_object_ref(bae6gb6)
			g_object_ref(bae6gb7)
			g_object_ref(bae6gb8)
			g_object_ref(bae6gb9)
			g_object_ref(bae6gb10)
			g_object_ref(bae6gl1)
			g_object_ref(bae6gl2)
			g_object_ref(bae6gl3)
			g_object_ref(bae6gl4)
			g_object_ref(bae6gl5)
			g_object_ref(bae6gl6)
			g_object_ref(bae6gl7)
			g_object_ref(bae6gl8)
			g_object_ref(bae6gl9)
			g_object_ref(bae6gl10)
			If bae6gab=TRUE Then
				bae6gab=FALSE
				gtk_button_set_label(bae6gridab, "+")
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb1)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb2)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb3)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb4)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb5)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb6)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb7)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb8)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb9)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb10)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl1)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl2)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl3)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl4)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl5)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl6)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl7)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl8)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl9)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gl10)
				gtk_grid_set_column_spacing (GTK_GRID(bae6grida),0)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb8, 3, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb9, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb10, 1, 2, 1, 1)
				gtk_widget_set_tooltip_text (bae6gb1,"Zoom In")
				gtk_widget_set_tooltip_text (bae6gb2,"Zoom Out")
				gtk_widget_set_tooltip_text (bae6gb3,"Pan")
				gtk_widget_set_tooltip_text (bae6gb4,"View Extents")
				gtk_widget_set_tooltip_text (bae6gb5,"Save View")
				gtk_widget_set_tooltip_text (bae6gb6,"Previous View")
				gtk_widget_set_tooltip_text (bae6gb7,"Next View")
				gtk_widget_set_tooltip_text (bae6gb8,"First View")
				gtk_widget_set_tooltip_text (bae6gb9,"Last View")
				gtk_widget_set_tooltip_text (bae6gb10,"Delete View")
			Else
				bae6gab=TRUE
				gtk_button_set_label(bae6gridab, "-")
				gtk_widget_set_has_tooltip(bae6gb1,FALSE)
				gtk_widget_set_has_tooltip(bae6gb2,FALSE)
				gtk_widget_set_has_tooltip(bae6gb3,FALSE)
				gtk_widget_set_has_tooltip(bae6gb4,FALSE)
				gtk_widget_set_has_tooltip(bae6gb5,FALSE)
				gtk_widget_set_has_tooltip(bae6gb6,FALSE)
				gtk_widget_set_has_tooltip(bae6gb7,FALSE)
				gtk_widget_set_has_tooltip(bae6gb8,FALSE)
				gtk_widget_set_has_tooltip(bae6gb9,FALSE)
				gtk_widget_set_has_tooltip(bae6gb10,FALSE)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb1)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb2)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb3)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb4)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb5)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb6)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb7)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb8)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb9)
				gtk_container_remove (GTK_GRID(bae6grida), bae6gb10)
				gtk_grid_set_column_spacing (GTK_GRID(bae6grida),10)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb8, 0, 7, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb9, 0, 8, 1, 1)
				gtk_grid_attach (GTK_GRID(bae6grida), bae6gb10, 0, 9, 1, 1)
				gtk_widget_set_halign (bae6gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl8, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl9, GTK_ALIGN_START)
				gtk_widget_set_halign (bae6gl10, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl1, bae6gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl2, bae6gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl3, bae6gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl4, bae6gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl5, bae6gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl6, bae6gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl7, bae6gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl8, bae6gb8, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl9, bae6gb9, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae6grida), bae6gl10, bae6gb10, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 7
			g_object_ref(bae7gb1)
			g_object_ref(bae7gb2)
			g_object_ref(bae7gb3)
			g_object_ref(bae7gb4)
			g_object_ref(bae7gb5)
			g_object_ref(bae7gb6)
			g_object_ref(bae7gb7)
			g_object_ref(bae7gl1)
			g_object_ref(bae7gl2)
			g_object_ref(bae7gl3)
			g_object_ref(bae7gl4)
			g_object_ref(bae7gl5)
			g_object_ref(bae7gl6)
			g_object_ref(bae7gl7)
			If bae7gab=TRUE Then
				bae7gab=FALSE
				gtk_button_set_label(bae7gridab, "+")
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb1)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb2)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb3)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb4)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb5)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb6)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb7)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl1)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl2)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl3)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl4)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl5)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl6)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gl7)
				gtk_grid_set_column_spacing (GTK_GRID(bae7grida),0)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb7, 2, 1, 1, 1)
				gtk_widget_set_tooltip_text (bae7gb1,"Dimension")
				gtk_widget_set_tooltip_text (bae7gb2,"X-Dimension")
				gtk_widget_set_tooltip_text (bae7gb3,"Y-Dimension")
				gtk_widget_set_tooltip_text (bae7gb4,"Radius-Dimension")
				gtk_widget_set_tooltip_text (bae7gb5,"Diameter-Dimension")
				gtk_widget_set_tooltip_text (bae7gb6,"Angle-Dimension")
				gtk_widget_set_tooltip_text (bae7gb7,"Dimension Properties")
			Else
				bae7gab=TRUE
				gtk_button_set_label(bae7gridab, "-")
				gtk_widget_set_has_tooltip(bae7gb1,FALSE)
				gtk_widget_set_has_tooltip(bae7gb2,FALSE)
				gtk_widget_set_has_tooltip(bae7gb3,FALSE)
				gtk_widget_set_has_tooltip(bae7gb4,FALSE)
				gtk_widget_set_has_tooltip(bae7gb5,FALSE)
				gtk_widget_set_has_tooltip(bae7gb6,FALSE)
				gtk_widget_set_has_tooltip(bae7gb7,FALSE)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb1)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb2)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb3)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb4)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb5)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb6)
				gtk_container_remove (GTK_GRID(bae7grida), bae7gb7)
				gtk_grid_set_column_spacing (GTK_GRID(bae7grida),10)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae7grida), bae7gb7, 0, 6, 1, 1)
				gtk_widget_set_halign (bae7gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae7gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae7gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae7gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae7gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae7gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae7gl7, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl1, bae7gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl2, bae7gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl3, bae7gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl4, bae7gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl5, bae7gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl6, bae7gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae7grida), bae7gl7, bae7gb7, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 8
			g_object_ref(bae8gb1)
			g_object_ref(bae8gb2)
			g_object_ref(bae8gb3)
			g_object_ref(bae8gb4)
			g_object_ref(bae8gb5)
			g_object_ref(bae8gb6)
			g_object_ref(bae8gb7)
			g_object_ref(bae8gb8)
			g_object_ref(bae8gb9)
			g_object_ref(bae8gb10)
			g_object_ref(bae8gl1)
			g_object_ref(bae8gl2)
			g_object_ref(bae8gl3)
			g_object_ref(bae8gl4)
			g_object_ref(bae8gl5)
			g_object_ref(bae8gl6)
			g_object_ref(bae8gl7)
			g_object_ref(bae8gl8)
			g_object_ref(bae8gl9)
			g_object_ref(bae8gl10)
			If bae8gab=TRUE Then
				bae8gab=FALSE
				gtk_button_set_label(bae8gridab, "+")
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb1)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb2)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb3)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb4)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb5)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb6)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb7)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb8)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb9)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb10)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl1)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl2)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl3)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl4)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl5)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl6)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl7)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl8)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl9)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gl10)
				gtk_grid_set_column_spacing (GTK_GRID(bae8grida),0)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb8, 3, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb9, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb10, 1, 2, 1, 1)
				gtk_widget_set_tooltip_text (bae8gb1,"Chamfer")
				gtk_widget_set_tooltip_text (bae8gb2,"Fillet")
				gtk_widget_set_tooltip_text (bae8gb3,"Best Curve Fit")
				gtk_widget_set_tooltip_text (bae8gb4,"Free hand C/F")
				gtk_widget_set_tooltip_text (bae8gb5,"Set user C/F length")
				gtk_widget_set_tooltip_text (bae8gb6,"Use C/F length(1) = " + Str(cham_fil_1))
				gtk_widget_set_tooltip_text (bae8gb7,"Use C/F length(2) = " + Str(cham_fil_2))
				gtk_widget_set_tooltip_text (bae8gb8,"Use C/F length(3) = " + Str(cham_fil_3))
				gtk_widget_set_tooltip_text (bae8gb9,"Use C/F length(4) = " + Str(cham_fil_4))
				gtk_widget_set_tooltip_text (bae8gb10,"Use C/F length(5) = " + Str(cham_fil_5))
			Else
				bae8gab=TRUE
				gtk_button_set_label(bae8gridab, "-")
				gtk_widget_set_has_tooltip(bae8gb1,FALSE)
				gtk_widget_set_has_tooltip(bae8gb2,FALSE)
				gtk_widget_set_has_tooltip(bae8gb3,FALSE)
				gtk_widget_set_has_tooltip(bae8gb4,FALSE)
				gtk_widget_set_has_tooltip(bae8gb5,FALSE)
				gtk_widget_set_has_tooltip(bae8gb6,FALSE)
				gtk_widget_set_has_tooltip(bae8gb7,FALSE)
				gtk_widget_set_has_tooltip(bae8gb8,FALSE)
				gtk_widget_set_has_tooltip(bae8gb9,FALSE)
				gtk_widget_set_has_tooltip(bae8gb10,FALSE)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb1)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb2)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb3)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb4)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb5)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb6)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb7)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb8)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb9)
				gtk_container_remove (GTK_GRID(bae8grida), bae8gb10)
				gtk_grid_set_column_spacing (GTK_GRID(bae8grida),10)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb8, 0, 7, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb9, 0, 8, 1, 1)
				gtk_grid_attach (GTK_GRID(bae8grida), bae8gb10, 0, 9, 1, 1)
				gtk_widget_set_halign (bae8gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl8, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl9, GTK_ALIGN_START)
				gtk_widget_set_halign (bae8gl10, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl1, bae8gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl2, bae8gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl3, bae8gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl4, bae8gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl5, bae8gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl6, bae8gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl7, bae8gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl8, bae8gb8, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl9, bae8gb9, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae8grida), bae8gl10, bae8gb10, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 9
			g_object_ref(bae9gb1)
			g_object_ref(bae9gb2)
			g_object_ref(bae9gb3)
			g_object_ref(bae9gb4)
			g_object_ref(bae9gl1)
			g_object_ref(bae9gl2)
			g_object_ref(bae9gl3)
			g_object_ref(bae9gl4)
			If bae9gab=TRUE Then
				bae9gab=FALSE
				gtk_button_set_label(bae9gridab, "+")
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb1)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb2)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb3)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb4)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gl1)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gl2)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gl3)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gl4)
				gtk_grid_set_column_spacing (GTK_GRID(bae9grida),0)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb4, 3, 0, 1, 1)
				gtk_widget_set_tooltip_text (bae9gb1,"Scale Properties")
				gtk_widget_set_tooltip_text (bae9gb2,"Scale Up")
				gtk_widget_set_tooltip_text (bae9gb3,"Scale Down")
				gtk_widget_set_tooltip_text (bae9gb4,"Scale w/ Grab Handles")
			Else
				bae9gab=TRUE
				gtk_button_set_label(bae9gridab, "-")
				gtk_widget_set_has_tooltip(bae9gb1,FALSE)
				gtk_widget_set_has_tooltip(bae9gb2,FALSE)
				gtk_widget_set_has_tooltip(bae9gb3,FALSE)
				gtk_widget_set_has_tooltip(bae9gb4,FALSE)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb1)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb2)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb3)
				gtk_container_remove (GTK_GRID(bae9grida), bae9gb4)
				gtk_grid_set_column_spacing (GTK_GRID(bae9grida),10)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae9grida), bae9gb4, 0, 3, 1, 1)
				gtk_widget_set_halign (bae9gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae9gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae9gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae9gl4, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae9grida), bae9gl1, bae9gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae9grida), bae9gl2, bae9gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae9grida), bae9gl3, bae9gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae9grida), bae9gl4, bae9gb4, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 10
			g_object_ref(bae10gb1)
			g_object_ref(bae10gb2)
			g_object_ref(bae10gb3)
			g_object_ref(bae10gb4)
			g_object_ref(bae10gl1)
			g_object_ref(bae10gl2)
			g_object_ref(bae10gl3)
			g_object_ref(bae10gl4)
			If bae10gab=TRUE Then
				bae10gab=FALSE
				gtk_button_set_label(bae10gridab, "+")
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb1)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb2)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb3)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb4)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gl1)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gl2)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gl3)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gl4)
				gtk_grid_set_column_spacing (GTK_GRID(bae10grida),0)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb4, 3, 0, 1, 1)
				gtk_widget_set_tooltip_text (bae10gb1,"Create Block")
				gtk_widget_set_tooltip_text (bae10gb2,"Insert Block")
				gtk_widget_set_tooltip_text (bae10gb3,"Explode Block")
				gtk_widget_set_tooltip_text (bae10gb4,"Edit Block")
			Else
				bae10gab=TRUE
				gtk_button_set_label(bae10gridab, "-")
				gtk_widget_set_has_tooltip(bae10gb1,FALSE)
				gtk_widget_set_has_tooltip(bae10gb2,FALSE)
				gtk_widget_set_has_tooltip(bae10gb3,FALSE)
				gtk_widget_set_has_tooltip(bae10gb4,FALSE)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb1)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb2)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb3)
				gtk_container_remove (GTK_GRID(bae10grida), bae10gb4)
				gtk_grid_set_column_spacing (GTK_GRID(bae10grida),10)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae10grida), bae10gb4, 0, 3, 1, 1)
				gtk_widget_set_halign (bae10gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae10gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae10gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae10gl4, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae10grida), bae10gl1, bae10gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae10grida), bae10gl2, bae10gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae10grida), bae10gl3, bae10gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae10grida), bae10gl4, bae10gb4, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 11
			g_object_ref(bae11gb1)
			g_object_ref(bae11gb2)
			g_object_ref(bae11gb3)
			g_object_ref(bae11gl1)
			g_object_ref(bae11gl2)
			g_object_ref(bae11gl3)
			If bae11gab=TRUE Then
				bae11gab=FALSE
				gtk_button_set_label(bae11gridab, "+")
				gtk_container_remove (GTK_GRID(bae11grida), bae11gb1)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gb2)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gb3)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gl1)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gl2)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gl3)
				gtk_grid_set_column_spacing (GTK_GRID(bae11grida),0)
				gtk_grid_attach (GTK_GRID(bae11grida), bae11gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae11grida), bae11gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae11grida), bae11gb3, 2, 0, 1, 1)
				gtk_widget_set_tooltip_text (bae11gb1,"Grid")
				gtk_widget_set_tooltip_text (bae11gb2,"Snap to Grid")
				gtk_widget_set_tooltip_text (bae11gb3,"Grid Properties")
			Else
				bae11gab=TRUE
				gtk_button_set_label(bae11gridab, "-")
				gtk_widget_set_has_tooltip(bae11gb1,FALSE)
				gtk_widget_set_has_tooltip(bae11gb2,FALSE)
				gtk_widget_set_has_tooltip(bae11gb3,FALSE)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gb1)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gb2)
				gtk_container_remove (GTK_GRID(bae11grida), bae11gb3)
				gtk_grid_set_column_spacing (GTK_GRID(bae11grida),10)
				gtk_grid_attach (GTK_GRID(bae11grida), bae11gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae11grida), bae11gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae11grida), bae11gb3, 0, 2, 1, 1)
				gtk_widget_set_halign (bae11gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae11gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae11gl3, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae11grida), bae11gl1, bae11gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae11grida), bae11gl2, bae11gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae11grida), bae11gl3, bae11gb3, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 12
			g_object_ref(bae12gb1)
			g_object_ref(bae12gb2)
			g_object_ref(bae12gb3)
			g_object_ref(bae12gb4)
			g_object_ref(bae12gl1)
			g_object_ref(bae12gl2)
			g_object_ref(bae12gl3)
			g_object_ref(bae12gl4)
			If bae12gab=TRUE Then
				bae12gab=FALSE
				gtk_button_set_label(bae12gridab, "+")
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb1)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb2)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb3)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb4)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gl1)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gl2)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gl3)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gl4)
				gtk_grid_set_column_spacing (GTK_GRID(bae12grida),0)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb4, 3, 0, 1, 1)
				gtk_widget_set_tooltip_text (bae12gb1,"G-Code")
				gtk_widget_set_tooltip_text (bae12gb2,"G-Code Properties")
				gtk_widget_set_tooltip_text (bae12gb3,"Reserved")
				gtk_widget_set_tooltip_text (bae12gb4,"Reserved")
			Else
				bae12gab=TRUE
				gtk_button_set_label(bae12gridab, "-")
				gtk_widget_set_has_tooltip(bae12gb1,FALSE)
				gtk_widget_set_has_tooltip(bae12gb2,FALSE)
				gtk_widget_set_has_tooltip(bae12gb3,FALSE)
				gtk_widget_set_has_tooltip(bae12gb4,FALSE)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb1)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb2)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb3)
				gtk_container_remove (GTK_GRID(bae12grida), bae12gb4)
				gtk_grid_set_column_spacing (GTK_GRID(bae12grida),10)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae12grida), bae12gb4, 0, 3, 1, 1)
				gtk_widget_set_halign (bae12gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae12gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae12gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae12gl4, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae12grida), bae12gl1, bae12gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae12grida), bae12gl2, bae12gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae12grida), bae12gl3, bae12gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae12grida), bae12gl4, bae12gb4, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 13
			g_object_ref(bae13gb1)
			g_object_ref(bae13gb2)
			g_object_ref(bae13gb3)
			g_object_ref(bae13gb4)
			g_object_ref(bae13gl1)
			g_object_ref(bae13gl2)
			g_object_ref(bae13gl3)
			g_object_ref(bae13gl4)
			If bae13gab=TRUE Then
				bae13gab=FALSE
				gtk_button_set_label(bae13gridab, "+")
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb1)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb2)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb3)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb4)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gl1)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gl2)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gl3)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gl4)
				gtk_grid_set_column_spacing (GTK_GRID(bae13grida),0)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb4, 3, 0, 1, 1)
				gtk_widget_set_tooltip_text (bae13gb1,"Move Point")
				gtk_widget_set_tooltip_text (bae13gb2,"Join Points")
				gtk_widget_set_tooltip_text (bae13gb3,"Trim")
				gtk_widget_set_tooltip_text (bae13gb4,"Extend")
			Else
				bae13gab=TRUE
				gtk_button_set_label(bae13gridab, "-")
				gtk_widget_set_has_tooltip(bae13gb1,FALSE)
				gtk_widget_set_has_tooltip(bae13gb2,FALSE)
				gtk_widget_set_has_tooltip(bae13gb3,FALSE)
				gtk_widget_set_has_tooltip(bae13gb4,FALSE)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb1)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb2)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb3)
				gtk_container_remove (GTK_GRID(bae13grida), bae13gb4)
				gtk_grid_set_column_spacing (GTK_GRID(bae13grida),10)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae13grida), bae13gb4, 0, 3, 1, 1)
				gtk_widget_set_halign (bae13gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae13gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae13gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae13gl4, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae13grida), bae13gl1, bae13gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae13grida), bae13gl2, bae13gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae13grida), bae13gl3, bae13gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae13grida), bae13gl4, bae13gb4, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 14
			g_object_ref(bae14gb1)
			g_object_ref(bae14gb2)
			g_object_ref(bae14gb3)
			g_object_ref(bae14gb4)
			g_object_ref(bae14gb5)
			g_object_ref(bae14gb6)
			g_object_ref(bae14gb7)
			g_object_ref(bae14gb8)
			g_object_ref(bae14gb9)
			g_object_ref(bae14gl1)
			g_object_ref(bae14gl2)
			g_object_ref(bae14gl3)
			g_object_ref(bae14gl4)
			g_object_ref(bae14gl5)
			g_object_ref(bae14gl6)
			g_object_ref(bae14gl7)
			g_object_ref(bae14gl8)
			g_object_ref(bae14gl9)
			If bae14gab=TRUE Then
				bae14gab=FALSE
				gtk_button_set_label(bae14gridab, "+")
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb1)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb2)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb3)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb4)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb5)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb6)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb7)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb8)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb9)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl1)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl2)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl3)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl4)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl5)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl6)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl7)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl8)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gl9)
				gtk_grid_set_column_spacing (GTK_GRID(bae14grida),0)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb7, 2, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb8, 3, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb9, 0, 2, 1, 1)
				gtk_widget_set_tooltip_text (bae14gb1,"Parallel Single")
				gtk_widget_set_tooltip_text (bae14gb2,"Parallel Dual")
				gtk_widget_set_tooltip_text (bae14gb3,"Set Parallel Offsets")
				gtk_widget_set_tooltip_text (bae14gb4,"Use Parallel Offset 1")
				gtk_widget_set_tooltip_text (bae14gb5,"Use Parallel Offset 2")
				gtk_widget_set_tooltip_text (bae14gb6,"Use Parallel Offset 3")
				gtk_widget_set_tooltip_text (bae14gb7,"Extend Exterior Parrallel Corners")
				gtk_widget_set_tooltip_text (bae14gb8,"Chamfer Parallel corners")
				gtk_widget_set_tooltip_text (bae14gb9,"Fillet Parallel corners")
			Else
				bae14gab=TRUE
				gtk_button_set_label(bae14gridab, "-")
				gtk_widget_set_has_tooltip(bae14gb1,FALSE)
				gtk_widget_set_has_tooltip(bae14gb2,FALSE)
				gtk_widget_set_has_tooltip(bae14gb3,FALSE)
				gtk_widget_set_has_tooltip(bae14gb4,FALSE)
				gtk_widget_set_has_tooltip(bae14gb5,FALSE)
				gtk_widget_set_has_tooltip(bae14gb6,FALSE)
				gtk_widget_set_has_tooltip(bae14gb7,FALSE)
				gtk_widget_set_has_tooltip(bae14gb8,FALSE)
				gtk_widget_set_has_tooltip(bae14gb9,FALSE)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb1)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb2)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb3)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb4)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb5)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb6)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb7)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb8)
				gtk_container_remove (GTK_GRID(bae14grida), bae14gb9)
				gtk_grid_set_column_spacing (GTK_GRID(bae14grida),10)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb7, 0, 6, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb8, 0, 7, 1, 1)
				gtk_grid_attach (GTK_GRID(bae14grida), bae14gb9, 0, 8, 1, 1)
				gtk_widget_set_halign (bae14gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl7, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl8, GTK_ALIGN_START)
				gtk_widget_set_halign (bae14gl9, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl1, bae14gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl2, bae14gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl3, bae14gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl4, bae14gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl5, bae14gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl6, bae14gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl7, bae14gb7, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl8, bae14gb8, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae14grida), bae14gl9, bae14gb9, GTK_POS_RIGHT, 1, 1)
			EndIf
		Case 15
			g_object_ref(bae15gb1)
			g_object_ref(bae15gb2)
			g_object_ref(bae15gb3)
			g_object_ref(bae15gb4)
			g_object_ref(bae15gb5)
			g_object_ref(bae15gb6)
			g_object_ref(bae15gb7)
			g_object_ref(bae15gl1)
			g_object_ref(bae15gl2)
			g_object_ref(bae15gl3)
			g_object_ref(bae15gl4)
			g_object_ref(bae15gl5)
			g_object_ref(bae15gl6)
			g_object_ref(bae15gl7)
			If bae15gab=TRUE Then
				bae15gab=FALSE
				gtk_button_set_label(bae15gridab, "+")
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb1)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb2)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb3)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb4)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb5)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb6)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb7)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl1)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl2)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl3)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl4)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl5)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl6)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gl7)
				gtk_grid_set_column_spacing (GTK_GRID(bae15grida),0)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb2, 1, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb3, 2, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb4, 3, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb5, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb6, 1, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb7, 2, 1, 1, 1)
				gtk_widget_set_tooltip_text (bae15gb1,"Text")
				gtk_widget_set_tooltip_text (bae15gb2,"Set Preset Text Sizes")
				gtk_widget_set_tooltip_text (bae15gb3,"Use Text Size(1)")
				gtk_widget_set_tooltip_text (bae15gb4,"Use Text Size(2)")
				gtk_widget_set_tooltip_text (bae15gb5,"Use Text Size(3)")
				gtk_widget_set_tooltip_text (bae15gb6,"Use Text Size(4)")
				gtk_widget_set_tooltip_text (bae15gb7,"Use Text Size(5)")
			Else
				bae15gab=TRUE
				gtk_button_set_label(bae15gridab, "-")
				gtk_widget_set_has_tooltip(bae15gb1,FALSE)
				gtk_widget_set_has_tooltip(bae15gb2,FALSE)
				gtk_widget_set_has_tooltip(bae15gb3,FALSE)
				gtk_widget_set_has_tooltip(bae15gb4,FALSE)
				gtk_widget_set_has_tooltip(bae15gb5,FALSE)
				gtk_widget_set_has_tooltip(bae15gb6,FALSE)
				gtk_widget_set_has_tooltip(bae15gb7,FALSE)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb1)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb2)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb3)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb4)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb5)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb6)
				gtk_container_remove (GTK_GRID(bae15grida), bae15gb7)
				gtk_grid_set_column_spacing (GTK_GRID(bae15grida),10)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb2, 0, 1, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb3, 0, 2, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb4, 0, 3, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb5, 0, 4, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb6, 0, 5, 1, 1)
				gtk_grid_attach (GTK_GRID(bae15grida), bae15gb7, 0, 6, 1, 1)
				gtk_widget_set_halign (bae15gl1, GTK_ALIGN_START)
				gtk_widget_set_halign (bae15gl2, GTK_ALIGN_START)
				gtk_widget_set_halign (bae15gl3, GTK_ALIGN_START)
				gtk_widget_set_halign (bae15gl4, GTK_ALIGN_START)
				gtk_widget_set_halign (bae15gl5, GTK_ALIGN_START)
				gtk_widget_set_halign (bae15gl6, GTK_ALIGN_START)
				gtk_widget_set_halign (bae15gl7, GTK_ALIGN_START)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl1, bae15gb1, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl2, bae15gb2, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl3, bae15gb3, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl4, bae15gb4, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl5, bae15gb5, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl6, bae15gb6, GTK_POS_RIGHT, 1, 1)
				gtk_grid_attach_next_to (GTK_GRID (bae15grida), bae15gl7, bae15gb7, GTK_POS_RIGHT, 1, 1)
			EndIf

		'add code for button area - add additional select case for expander

	End Select
End Sub
Sub dim_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If dimbuttonclicked=TRUE Then Exit Sub
	dimbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=71
			buttonmanager
		Case 2
			selbutton=72
			buttonmanager
		Case 3
			selbutton=73
			buttonmanager
		Case 4
			selbutton=74
			buttonmanager
		Case 5
			selbutton=75
			buttonmanager
		Case 6
			selbutton=76
			buttonmanager
		Case 7
			mytextbuffer="Dimension Properties"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3'dialog grid label(n)
			Dim As GtkWidget PTR dge1,dge2,dge3'dialog grid entry(n)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Dimension Properties")
			'gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 100)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Precision ")
			dgl2 = gtk_label_new ("Arrow Size ")
			dgl3 = gtk_label_new ("Leader Offset ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,2)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(dimension_precision))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(dimension_arrow_size))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(dimension_leader_offset))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (page5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				dimension_precision=ValInt(*gtk_entry_get_text(dge1))
				dimension_arrow_size=Val(*gtk_entry_get_text(dge2))
				dimension_leader_offset=Val(*gtk_entry_get_text(dge3))
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
	End Select
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	dimbuttonclicked=FALSE
End Sub
Sub cham_fil_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If chamfilbuttonclicked=TRUE Then Exit Sub
	chamfilbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=81
			buttonmanager
		Case 2
			selbutton=82
			buttonmanager
		Case 3
			selbutton=83
			buttonmanager
		Case 4
			selbutton=84
			buttonmanager
		Case 5
			mytextbuffer="Set user C/F length"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3,dgl4,dgl5'dialog grid label(1-5)
			Dim As GtkWidget PTR dge1,dge2,dge3,dge4,dge5'dialog grid entry(1-5)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Set user C/F length")
			'gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			gtk_window_set_default_size(GTK_WINDOW(dialog), 170, 100)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("C/F length(1) ")
			dgl2 = gtk_label_new ("C/F length(2) ")
			dgl3 = gtk_label_new ("C/F length(3) ")
			dgl4 = gtk_label_new ("C/F length(4) ")
			dgl5 = gtk_label_new ("C/F length(5) ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			dge4 = gtk_entry_new()
			dge5 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,10)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,10)
			gtk_entry_set_width_chars (dge4,10)
			gtk_entry_set_width_chars (dge5,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(cham_fil_1))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(cham_fil_2))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(cham_fil_3))
			gtk_entry_set_text(GTK_ENTRY(dge4),Str(cham_fil_4))
			gtk_entry_set_text(GTK_ENTRY(dge5),Str(cham_fil_5))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl4, 0, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl5, 0, 4, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge4, 1, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge5, 1, 4, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				cham_fil_1=Val(*gtk_entry_get_text(dge1))
				cham_fil_2=Val(*gtk_entry_get_text(dge2))
				cham_fil_3=Val(*gtk_entry_get_text(dge3))
				cham_fil_4=Val(*gtk_entry_get_text(dge4))
				cham_fil_5=Val(*gtk_entry_get_text(dge5))
				gtk_widget_set_tooltip_text (bae8gb6,"Use C/F length(1) = " + Str(cham_fil_1))
				gtk_widget_set_tooltip_text (bae8gb7,"Use C/F length(2) = " + Str(cham_fil_2))
				gtk_widget_set_tooltip_text (bae8gb8,"Use C/F length(3) = " + Str(cham_fil_3))
				gtk_widget_set_tooltip_text (bae8gb9,"Use C/F length(4) = " + Str(cham_fil_4))
				gtk_widget_set_tooltip_text (bae8gb10,"Use C/F length(5) = " + Str(cham_fil_5))
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
		Case 6
			selbutton=86
			buttonmanager
		Case 7
			selbutton=87
			buttonmanager
		Case 8
			selbutton=88
			buttonmanager
		Case 9
			selbutton=89
			buttonmanager
		Case 10
			selbutton=90
			buttonmanager
	End Select
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	chamfilbuttonclicked=FALSE
End Sub
Sub scale_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If scalebuttonclicked=TRUE Then Exit Sub
	scalebuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			mytextbuffer="Scale Properties"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2'dialog grid label(n)
			Dim As GtkWidget PTR dge1,dge2'dialog grid entry(n)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Scale Properties")
			'gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 100)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Scale Factor(X) ")
			dgl2 = gtk_label_new ("Scale Factor(Y) ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,2)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(scale_factor_x))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(scale_factor_y))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (page5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				scale_factor_x=Val(*gtk_entry_get_text(dge1))
				scale_factor_y=Val(*gtk_entry_get_text(dge2))
				If scale_factor_x>0 Then scalexfactor=scale_factor_x
				If scale_factor_y>0 Then scaleyfactor=scale_factor_y
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
		Case 2
			selbutton=103
			buttonmanager
		Case 3
			selbutton=104
			buttonmanager
		Case 4
			selbutton=109
			buttonmanager
	End Select
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	scalebuttonclicked=FALSE
End Sub
Sub block_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If blockbuttonclicked=TRUE Or disable_all_block_buttons=TRUE Then Exit Sub
	blockbuttonclicked=TRUE
	Dim As Integer i,j,k,c,response
	Dim As BOOLEAN duplicate_block_name,toggle_state,get_next_block_number
	blockname=""
	i=CAST(Integer, dat)
	Select Case i
		Case 1'block
			'show dialog box to Create Block
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2'dialog grid label(n)
			Dim As GtkWidget PTR dge1'dialog grid entry(n)
			Dim As GtkWidget PTR dgcb1'dialog grid checkbox(n)
			Dim As String next_available_block_number_name
			
			next_available_block_number_name="block"+LTrim(Str(next_block_number))
			Do
				get_next_block_number=FALSE
				For i=1 To blockc
					If blocknames(i)=next_available_block_number_name Then
						get_next_block_number=TRUE
						Exit For
					EndIf
				Next
				If get_next_block_number=TRUE Then
					next_block_number+=1
					next_available_block_number_name="block"+LTrim(Str(next_block_number))
				Else
					Exit Do
				EndIf
			Loop
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Create Block")
			gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Enter Block Name ")
			dgl2 = gtk_label_new ("Enable Base Point ")
			dge1 = gtk_entry_new()
			dgcb1 = gtk_check_button_new()
			gtk_entry_set_width_chars (dge1,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(next_available_block_number_name))
			'gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dgcb1), TRUE)

			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgcb1, 1, 1, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			Select Case response
				Case -4
					
				Case -5
					'create / save block with base point
					If *gtk_entry_get_text(dge1) = next_available_block_number_name Then next_block_number+=1
					blockname = *gtk_entry_get_text(dge1)
			End Select
			
			buttonson(113)=FALSE
			If blockname<>"" Then
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dgcb1))
				If toggle_state=TRUE Then
					buttonson(113)=TRUE
					theboxbelow("toggle on")
				EndIf
				gtk_widget_destroy(dialog)
				selbutton=111
				buttonmanager
			Else
				gtk_widget_destroy(dialog)
			EndIf
		Case 2'insert block
			ReDim tempstrarray(blockc)
			c=0
			For j=1 To blockc
				duplicate_block_name=FALSE
				For k=1 To c
					If blocknames(j)=tempstrarray(k) Then
						duplicate_block_name=TRUE
						Exit For
					EndIf
				Next
				If duplicate_block_name=FALSE Then
					c+=1
					tempstrarray(c)=blocknames(j)
				EndIf
			Next
			
			
			'lists INTERNAL Block names (blocks that are already part of the drawing)
			'optionally EXTernal blocks can be inserted via open dialog box
			'show dialog box to Insert Block
			Dim As GtkWidget PTR insert_block_dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2'dialog grid label(n)
			Dim As GtkWidget PTR dgcombobox
			Dim As String combotext
			Dim As Integer blocknamesi
			
			insert_block_dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( insert_block_dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(insert_block_dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(insert_block_dialog), "Insert Block")
			gtk_window_set_resizable (GTK_WINDOW(insert_block_dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(insert_block_dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(insert_block_dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Select Block Name or enter ")
			dgl2 = gtk_label_new ("block name to Import Block ")
			dgcombobox = gtk_combo_box_text_new_with_entry ( )

			For blocknamesi=1 To c
			   gtk_combo_box_text_append(GTK_COMBO_BOX_TEXT(dgcombobox), NULL, tempstrarray(blocknamesi))
			Next
		   gtk_combo_box_set_active (GTK_COMBO_BOX(dgcombobox), 0)
			
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgcombobox, 0, 2, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(insert_block_dialog), "Insert", GTK_RESPONSE_OK)
			gtk_dialog_add_button(GTK_DIALOG(insert_block_dialog), "Search", GTK_RESPONSE_YES)
			
			'gtk_widget_grab_focus (dge5)
			'gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(insert_block_dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (insert_block_dialog)
			response = gtk_dialog_run(GTK_DIALOG(insert_block_dialog))
			Select Case response
				Case -4
					
				Case -5
					'insert block
					combotext = *gtk_combo_box_text_get_active_text(GTK_COMBO_BOX_TEXT(dgcombobox))
					blockname=combotext
					If LCase(Right(blockname,4))=".dxf" Then blockname=Left(blockname,Len(blockname)-4)
				Case -8
					Dim as GtkWidget ptr dialog
					gtk_init( 0, NULL )
					dialog = gtk_file_chooser_dialog_new( "Open File", _
																		NULL, _
																		GTK_FILE_CHOOSER_ACTION_OPEN, _
																		GTK_STOCK_CANCEL, GTK_RESPONSE_CANCEL, _
																		GTK_STOCK_OPEN, GTK_RESPONSE_ACCEPT, _
																		NULL )
					If( gtk_dialog_run( GTK_DIALOG( dialog ) ) = GTK_RESPONSE_ACCEPT ) then
						Dim as zstring ptr filename
						filename = gtk_file_chooser_get_filename( GTK_FILE_CHOOSER( dialog ) )
						blockname=*filename
						If LCase(Right(blockname,4))=".dxf" Then blockname=Left(blockname,Len(blockname)-4)
					End if
					gtk_widget_destroy( GTK_WIDGET(dialog) )
				
			End Select
			gtk_widget_destroy(insert_block_dialog)
			If blockname<>"" Then
				selbutton=112
				buttonmanager
			EndIf
		Case 3'explode block
			selbutton=114
			buttonmanager
		Case 4'edit block
			If buttonson(116)=TRUE Then
				If edit_master_block=TRUE Then
					'edit block button was turned off prior to block edit update completion
				EndIf
				selbutton=116
				buttonmanager
			Else
				if groupexists=false Then
					theboxbelow("groupexits=false")
				EndIf
				theboxbelow("edit master block")
				edit_master_block_name=""
				Dim As Integer i,j,k,blocknumber
				Dim As BOOLEAN multipart_selection
				blocknumber=0
				For i = preloadedlinec+1 To linec 
					'If lines(i,9)=lines(selline,9) Then lines(i,8)=1
					If lines(i,8)=1 Then
						blocknumber=lines(i,9)
						Exit For
					EndIf
				Next
				If blocknumber=0 Then
					For i = preloadedcirclec+1 To circlec 
						If circles(i,10)=1 Then
							blocknumber=circles(i,12)
							Exit For
						EndIf
					Next
				EndIf
				If blocknumber=0 Then
					'no blocks in group found
					theboxbelow("no blocks found")
				Else
					'rescan for multi blocks in group
					multipart_selection=FALSE
					For i = preloadedlinec+1 To linec 
						'If lines(i,9)=lines(selline,9) Then lines(i,8)=1
						If lines(i,8)=1 Then
							If lines(i,9)<>blocknumber Then
								theboxbelow("Multi Part Selection. Please select a block to edit (Only one block at a time. The selection can not contain anything other than the one block)")
								multipart_selection=TRUE
								Exit For
							EndIf
						EndIf
					Next
					If multipart_selection=FALSE Then
						For i = preloadedcirclec+1 To circlec
							If circles(i,10)=1 Then
								If circles(i,12)<>blocknumber Then
									theboxbelow("Multi Part Selection. Please select a block to edit (Only one block at a time. The selection can not contain anything other than the one block)")
									multipart_selection=TRUE
									Exit For
								EndIf
							EndIf
						Next
					EndIf
				EndIf
				If multipart_selection=TRUE Then
					theboxbelow("Multipart selection=true. Please select a block to edit (ONLY and JUST the one block) ")
				Else
					For i=1 To blockc
						If blocknames(i)=blocknames(blocknumber) Then
							Dim As GtkWidget PTR dialog,content
							Dim As GtkWidget PTR dg'dialog grid
							Dim As GtkWidget PTR dgl1,dgl2'dialog grid label(n)
							Dim As GtkWidget PTR dge1'dialog grid entry(n)
							Dim As GtkWidget PTR dgcb1'dialog grid checkbox(n)
							
							dialog = gtk_dialog_new()
							gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
							gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
							gtk_window_set_title(GTK_WINDOW(dialog), "Edit Master Block")
							gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
							'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
							content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
							
							dg = gtk_grid_new()
							dgl1 = gtk_label_new ("Enter Block Name ")
							dgl2 = gtk_label_new ("Enable Base Point ")
							dge1 = gtk_entry_new()
							dgcb1 = gtk_check_button_new()
							gtk_entry_set_width_chars (dge1,10)
							gtk_entry_set_text(GTK_ENTRY(dge1),blocknames(blocknumber))
							'gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(dgcb1), TRUE)
				
							gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
							gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
							gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
							gtk_grid_attach (GTK_GRID(dg), dgcb1, 1, 1, 1, 1)
							
							gtk_container_add (GTK_CONTAINER(content), dg)
							gtk_dialog_add_button(GTK_DIALOG(dialog), "Cancle", GTK_RESPONSE_CANCEL)
							gtk_dialog_add_button(GTK_DIALOG(dialog), "Edit", GTK_RESPONSE_OK)
							
							'gtk_widget_grab_focus (dge5)
							gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
							gtk_widget_show_all (dialog)
							response = gtk_dialog_run(GTK_DIALOG(dialog))
							Select Case response
								Case -6'cancel
									gtk_widget_destroy(dialog)
									Exit For
								Case -5
									edit_master_block_name=blocknames(i)
							End Select
							
							buttonson(113)=FALSE
							If blockname<>"" Then
								toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(dgcb1))
								If toggle_state=TRUE Then
									buttonson(113)=TRUE
									theboxbelow("toggle on")
								EndIf
								gtk_widget_destroy(dialog)
							Else
								gtk_widget_destroy(dialog)
							EndIf
							Exit For
						EndIf
					Next
					If edit_master_block_name<>"" Then
						selbutton=116
						buttonmanager
					EndIf
				EndIf
			EndIf
	End Select
	blockbuttonclicked=FALSE
End Sub
Sub grid_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If gridbuttonclicked=TRUE Then Exit Sub
	gridbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=121
		Case 2
			selbutton=122
		Case 3
			'Grid Properties dialog
			mytextbuffer="Grid Properties"+Chr(10)+mytextbuffer

			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3,dgl4'dialog grid label(n)
			Dim As GtkWidget PTR dge1,dge2,dge3,dge4'dialog grid entry(n)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Grid Properties")
			'gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 100)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Grid Spacing(X) ")
			dgl2 = gtk_label_new ("Grid Spacing(Y) ")
			dgl3 = gtk_label_new ("Grid Offset(X) ")
			dgl4 = gtk_label_new ("Grid Offset(Y) ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			dge4 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,2)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,2)
			gtk_entry_set_width_chars (dge4,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(grid_spacing_x))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(grid_spacing_y))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(grid_offset_x))
			gtk_entry_set_text(GTK_ENTRY(dge4),Str(grid_offset_y))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl4, 0, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge4, 1, 3, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (page5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				grid_spacing_x=Val(*gtk_entry_get_text(dge1))
				grid_spacing_y=Val(*gtk_entry_get_text(dge2))
				grid_offset_x=Val(*gtk_entry_get_text(dge3))
				grid_offset_y=Val(*gtk_entry_get_text(dge4))
				gridxspacing=grid_spacing_x
				gridyspacing=grid_spacing_y
				gridxoffset=grid_offset_x
				gridyoffset=grid_offset_y
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
			selbutton=121
			turnbuttonoff2
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	gridbuttonclicked=FALSE
End Sub
Sub gcode_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If gcodebuttonclicked=TRUE Then Exit Sub
	gcodebuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=141
			gseqn=0
		Case 2
			selbutton=142
			
			mytextbuffer="Gcode Properties"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3,dgl4,dgl5,dgl6'dialog grid label(1-6)
			Dim As GtkWidget PTR dge1,dge2,dge3,dge4,dge5,dge6'dialog grid entry(1-6)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Text Size")
			gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Z axis plunge depth ")
			dgl2 = gtk_label_new ("Z axis steps ")
			dgl3 = gtk_label_new ("G1 z feed rate ")
			dgl4 = gtk_label_new ("G1 xy feed rate ")
			dgl5 = gtk_label_new ("Units: enter 20 for inches or 21 for millimeters ")
			dgl6 = gtk_label_new ("Curve degrees of resolution (recommended range 1 to 3 but can be less or more ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			dge4 = gtk_entry_new()
			dge5 = gtk_entry_new()
			dge6 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,10)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,10)
			gtk_entry_set_width_chars (dge4,10)
			gtk_entry_set_width_chars (dge5,10)
			gtk_entry_set_width_chars (dge6,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(gcode_z_axis_plunge_depth))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(gcode_Z_axis_steps))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(gcode_z_feed_rate))
			gtk_entry_set_text(GTK_ENTRY(dge4),Str(gcode_xy_feed_rate))
			gtk_entry_set_text(GTK_ENTRY(dge5),Str(gcode_units))
			gtk_entry_set_text(GTK_ENTRY(dge6),Str(gcode_curve_resolution))
			
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl4, 0, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl5, 0, 4, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl6, 0, 5, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge4, 1, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge5, 1, 4, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge6, 1, 5, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				gcode_z_axis_plunge_depth=Val(*gtk_entry_get_text(dge1))
				gcode_Z_axis_steps=       Val(*gtk_entry_get_text(dge2))
				gcode_z_feed_rate=        Val(*gtk_entry_get_text(dge3))
				gcode_xy_feed_rate=       Val(*gtk_entry_get_text(dge4))
				gcode_units=              Val(*gtk_entry_get_text(dge5))
				gcode_curve_resolution=   Val(*gtk_entry_get_text(dge6))
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
			turnbuttonoff
		Case 3
			selbutton=143
		Case 4
			selbutton=144
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	gcodebuttonclicked=FALSE
End Sub
Sub misc_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If miscbuttonclicked=TRUE Then Exit Sub
	miscbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=51
		Case 2
			selbutton=52
		Case 3
			selbutton=61
		Case 4
			selbutton=62
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	miscbuttonclicked=FALSE
End Sub
Sub parallel_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If parallelbuttonclicked=TRUE Then Exit Sub
	parallelbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=55
			buttonmanager
		Case 2
			selbutton=56
			buttonmanager
		Case 3
			mytextbuffer="Set Parallel Offsets"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3'dialog grid label(1-5)
			Dim As GtkWidget PTR dge1,dge2,dge3'dialog grid entry(1-5)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Parallel Offsets")
			gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Offset(1) ")
			dgl2 = gtk_label_new ("Offset(2) ")
			dgl3 = gtk_label_new ("Offset(3) ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,10)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(parallel_offset_1))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(parallel_offset_2))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(parallel_offset_3))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				parallel_offset_1=Val(*gtk_entry_get_text(dge1))
				parallel_offset_2=Val(*gtk_entry_get_text(dge2))
				parallel_offset_3=Val(*gtk_entry_get_text(dge3))
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
		Case 4
			selbutton=58
			buttonmanager
		Case 5
			selbutton=59
			buttonmanager
		Case 6
			selbutton=60
			buttonmanager
		Case 7
			selbutton=65
			buttonmanager
		Case 8
			selbutton=67
			buttonmanager
		Case 9
			selbutton=66
			buttonmanager
	End Select
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	parallelbuttonclicked=FALSE
End Sub
Sub text_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If textbuttonclicked=TRUE Then Exit Sub
	textbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			mytextbuffer="Text"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1
			Dim As GtkWidget PTR dge1
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Text Size")
			gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Enter Text ")
			dge1 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,50)
			gtk_entry_set_text(GTK_ENTRY(dge1),"911 was an inside job")
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "OK", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				tempstring=*gtk_entry_get_text(dge1)
				selbutton=91
				buttonmanager
			EndIf	
			gtk_widget_destroy(dialog)

			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
		Case 2
			mytextbuffer="Set Text Size"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3,dgl4,dgl5'dialog grid label(1-5)
			Dim As GtkWidget PTR dge1,dge2,dge3,dge4,dge5'dialog grid entry(1-5)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Text Size")
			gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Text Size(1) ")
			dgl2 = gtk_label_new ("Text Size(2) ")
			dgl3 = gtk_label_new ("Text Size(3) ")
			dgl4 = gtk_label_new ("Text Size(4) ")
			dgl5 = gtk_label_new ("Text Size(5) ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			dge4 = gtk_entry_new()
			dge5 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,10)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,10)
			gtk_entry_set_width_chars (dge4,10)
			gtk_entry_set_width_chars (dge5,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(text_size_1))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(text_size_2))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(text_size_3))
			gtk_entry_set_text(GTK_ENTRY(dge4),Str(text_size_4))
			gtk_entry_set_text(GTK_ENTRY(dge5),Str(text_size_5))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl4, 0, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl5, 0, 4, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge4, 1, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge5, 1, 4, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				text_size_1=Val(*gtk_entry_get_text(dge1))
				text_size_2=Val(*gtk_entry_get_text(dge2))
				text_size_3=Val(*gtk_entry_get_text(dge3))
				text_size_4=Val(*gtk_entry_get_text(dge4))
				text_size_5=Val(*gtk_entry_get_text(dge5))
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
		Case 3
			selbutton=93
			buttonmanager
		Case 4
			selbutton=94
			buttonmanager
		Case 5
			selbutton=95
			buttonmanager
		Case 6
			selbutton=96
			buttonmanager
		Case 7
			selbutton=97
			buttonmanager
	End Select
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	textbuttonclicked=FALSE
End Sub
SUB zpv_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If zpvbuttonclicked=TRUE Then Exit Sub
	zpvbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=41
		Case 2
			selbutton=42
		Case 3
			selbutton=43
		Case 4
			selbutton=44
		Case 5
			selbutton=45
		Case 6
			selbutton=46
		Case 7
			selbutton=47
		Case 8
			selbutton=48
		Case 9
			selbutton=49
		Case 10
			selbutton=50
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	zpvbuttonclicked=FALSE
End Sub
Sub drawing_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If drawingbuttonclicked=TRUE Then Exit Sub
	drawingbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1 To 6
			selbutton=i
		Case 7
			selbutton=1001
		Case 8
			selbutton=1002
		Case 9
			selbutton=1003
		Case 10
			selbutton=1004
		Case 11
			selbutton=1005
		Case 12
			selbutton=1006
		Case 13
			selbutton=1007
		Case 14
			selbutton=1008
		Case 15
			selbutton=1009
		Case 16
			selbutton=1010
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	drawingbuttonclicked=FALSE
End Sub
SUB snap_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If snapbuttonclicked=TRUE Then Exit Sub
	snapbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=11
		Case 2
			selbutton=12
		Case 3
			selbutton=13
		Case 4
			selbutton=14
		Case 5
			selbutton=16
		Case 6
			selbutton=18
		Case 7
			selbutton=19
		Case 8
			selbutton=20
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	snapbuttonclicked=FALSE
End Sub
SUB mcrf_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If mcrfbuttonclicked=TRUE Then Exit Sub
	mcrfbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=31
		Case 2
			selbutton=32
		Case 3
			selbutton=33
		Case 4
			selbutton=34
		Case 5
			selbutton=35
		Case 6
			selbutton=36
		Case 7
			selbutton=37
		Case 8
			selbutton=38
		Case 9
			selbutton=39
		Case 10
			selbutton=40
	End Select
	buttonmanager
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	mcrfbuttonclicked=FALSE
End Sub
SUB ortho_angle_from_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If orthbuttonclicked=TRUE Then Exit Sub
	orthbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 1
			selbutton=21
		Case 2
			selbutton=22
			If buttonson(22)=FALSE And buttonson(24)=FALSE And buttonson(25)=FALSE And buttonson(26)=FALSE And buttonson(27)=FALSE And buttonson(28)=FALSE Then
				mytextbuffer="Set Angle"+Chr(10)+mytextbuffer
				gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
				'show dialog box to enter new preset angle
				Dim As GtkWidget PTR dialog,content
				Dim As GtkWidget PTR dg'dialog grid
				Dim As GtkWidget PTR dgl1
				Dim As GtkWidget PTR dge1
				
				Dim As Integer response
				dialog = gtk_dialog_new()
				gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
				gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
				gtk_window_set_title(GTK_WINDOW(dialog), "Set Angle")
				gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
				'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
				content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
				
				dg = gtk_grid_new()
				dgl1 = gtk_label_new ("Angle ")
				dge1 = gtk_entry_new()
				gtk_entry_set_width_chars (dge1,10)
				gtk_entry_set_text(GTK_ENTRY(dge1),Str(preset_angle_0))
				gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
				gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
				
				gtk_container_add (GTK_CONTAINER(content), dg)
				gtk_dialog_add_button(GTK_DIALOG(dialog), "OK", GTK_RESPONSE_OK)
				'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
				
				'gtk_widget_grab_focus (dge5)
				gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
				gtk_widget_show_all (dialog)
				response = gtk_dialog_run(GTK_DIALOG(dialog))
				If response=-5 Then
					preset_angle_0=Val(*gtk_entry_get_text(dge1))
					angle2=preset_angle_0
				Else
					angle2=0
				EndIf	
				gtk_widget_destroy(dialog)
				mytextbuffer=Str(response)+Chr(10)+mytextbuffer
			EndIf
		Case 3
			selbutton=23
			mytextbuffer="Set preset Angles"+Chr(10)+mytextbuffer
			gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dgl1,dgl2,dgl3,dgl4,dgl5'dialog grid label(1-5)
			Dim As GtkWidget PTR dge1,dge2,dge3,dge4,dge5'dialog grid entry(1-5)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), "Preset Angles")
			gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			'gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 200)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dgl1 = gtk_label_new ("Angle(1) ")
			dgl2 = gtk_label_new ("Angle(2) ")
			dgl3 = gtk_label_new ("Angle(3) ")
			dgl4 = gtk_label_new ("Angle(4) ")
			dgl5 = gtk_label_new ("Angle(5) ")
			dge1 = gtk_entry_new()
			dge2 = gtk_entry_new()
			dge3 = gtk_entry_new()
			dge4 = gtk_entry_new()
			dge5 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,10)
			gtk_entry_set_width_chars (dge2,10)
			gtk_entry_set_width_chars (dge3,10)
			gtk_entry_set_width_chars (dge4,10)
			gtk_entry_set_width_chars (dge5,10)
			gtk_entry_set_text(GTK_ENTRY(dge1),Str(preset_angle_1))
			gtk_entry_set_text(GTK_ENTRY(dge2),Str(preset_angle_2))
			gtk_entry_set_text(GTK_ENTRY(dge3),Str(preset_angle_3))
			gtk_entry_set_text(GTK_ENTRY(dge4),Str(preset_angle_4))
			gtk_entry_set_text(GTK_ENTRY(dge5),Str(preset_angle_5))
			gtk_grid_attach (GTK_GRID(dg), dgl1, 0, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl2, 0, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl3, 0, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl4, 0, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dgl5, 0, 4, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge2, 1, 1, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge3, 1, 2, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge4, 1, 3, 1, 1)
			gtk_grid_attach (GTK_GRID(dg), dge5, 1, 4, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "Save", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			'gtk_widget_grab_focus (dge5)
			gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				preset_angle_1=Val(*gtk_entry_get_text(dge1))
				preset_angle_2=Val(*gtk_entry_get_text(dge2))
				preset_angle_3=Val(*gtk_entry_get_text(dge3))
				preset_angle_4=Val(*gtk_entry_get_text(dge4))
				preset_angle_5=Val(*gtk_entry_get_text(dge5))
				usersetangles(1)=preset_angle_1
				usersetangles(2)=preset_angle_2
				usersetangles(3)=preset_angle_3
				usersetangles(4)=preset_angle_4
				usersetangles(5)=preset_angle_5
				gtk_widget_set_tooltip_text (bae4gb4,"Preset Angle(1) = " + Str(preset_angle_1))
				gtk_widget_set_tooltip_text (bae4gb5,"Preset Angle(2) = " + Str(preset_angle_2))
				gtk_widget_set_tooltip_text (bae4gb6,"Preset Angle(3) = " + Str(preset_angle_3))
				gtk_widget_set_tooltip_text (bae4gb7,"Preset Angle(4) = " + Str(preset_angle_4))
				gtk_widget_set_tooltip_text (bae4gb8,"Preset Angle(5) = " + Str(preset_angle_5))
			EndIf	
			gtk_widget_destroy(dialog)
			mytextbuffer=Str(response)+Chr(10)+mytextbuffer
		Case 4
			selbutton=24
		Case 5
			selbutton=25
		Case 6
			selbutton=26
		Case 7
			selbutton=27
		Case 8
			selbutton=28
		Case 9
			selbutton=53
		Case 10
			selbutton=54
	End Select
	buttonmanager

	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	orthbuttonclicked=FALSE
End Sub
SUB layer_state_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If layerstatebuttonclicked=TRUE Then Exit Sub
	layerstatebuttonclicked=TRUE
	Dim As BOOLEAN toggle_state
	Dim i As Integer
	i=CAST(INTEGER, dat)
	'3 laryerstates
	'layerstate 0 = hidden
	'layerstate 1 = visible not detectable
	'layerstate 2 = visible and detectable
	'layerstate(34)=2'34 is used as the color of rays
	'for y=1 to 15
	'	layerstate(y)=2
	'Next
	Select Case i
		Case 11
			If layer_active=1 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1b), TRUE)
				layerstate(1)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb1a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb1b))
					If toggle_state=TRUE Then
						layerstate(1)=2
					Else
						layerstate(1)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1b), FALSE)
					layerstate(1)=0
				EndIf
			EndIf
		Case 12
			If layer_active=1 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1b), TRUE)
				layerstate(1)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb1b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb1a))
					If toggle_state=TRUE Then
						layerstate(1)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1b), FALSE)
						layerstate(1)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb1a))
					If toggle_state=TRUE Then
						layerstate(1)=1
					Else
						layerstate(1)=0
					EndIf
				EndIf
			EndIf



		Case 21
			If layer_active=2 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2b), TRUE)
				layerstate(2)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb2a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb2b))
					If toggle_state=TRUE Then
						layerstate(2)=2
					Else
						layerstate(2)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2b), FALSE)
					layerstate(2)=0
				EndIf
			EndIf
		Case 22
			If layer_active=2 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2b), TRUE)
				layerstate(2)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb2b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb2a))
					If toggle_state=TRUE Then
						layerstate(2)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2b), FALSE)
						layerstate(2)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb2a))
					If toggle_state=TRUE Then
						layerstate(2)=1
					Else
						layerstate(2)=0
					EndIf
				EndIf
			EndIf




		'Case x1
		'	If layer_active=x Then
		'		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcbxa), TRUE)
		'		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcbxb), TRUE)
		'		layerstate(x)=2
		'	Else
		'		toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcbxa))
		'		If toggle_state=TRUE Then
		'			toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcbxb))
		'			If toggle_state=TRUE Then
		'				layerstate(x)=2
		'			Else
		'				layerstate(x)=1
		'			EndIf
		'		Else
		'			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcbxb), FALSE)
		'			layerstate(x)=0
		'		EndIf
		'	EndIf
		'Case x2
		'	If layer_active=x Then
		'		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcbxa), TRUE)
		'		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcbxb), TRUE)
		'		layerstate(x)=2
		'	Else
		'		toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcbxb))
		'		If toggle_state=TRUE Then
		'			toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcbxa))
		'			If toggle_state=TRUE Then
		'				layerstate(x)=2
		'			Else
		'				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcbxb), FALSE)
		'				layerstate(x)=0
		'			EndIf
		'		Else
		'			toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcbxa))
		'			If toggle_state=TRUE Then
		'				layerstate(x)=1
		'			Else
		'				layerstate(x)=0
		'			EndIf
		'		EndIf
		'	EndIf



		Case 31
			If layer_active=3 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3b), TRUE)
				layerstate(3)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb3a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb3b))
					If toggle_state=TRUE Then
						layerstate(3)=2
					Else
						layerstate(3)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3b), FALSE)
					layerstate(3)=0
				EndIf
			EndIf
		Case 32
			If layer_active=3 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3b), TRUE)
				layerstate(3)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb3b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb3a))
					If toggle_state=TRUE Then
						layerstate(3)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3b), FALSE)
						layerstate(3)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb3a))
					If toggle_state=TRUE Then
						layerstate(3)=1
					Else
						layerstate(3)=0
					EndIf
				EndIf
			EndIf




		Case 41
			If layer_active=4 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4b), TRUE)
				layerstate(4)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb4a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb4b))
					If toggle_state=TRUE Then
						layerstate(4)=2
					Else
						layerstate(4)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4b), FALSE)
					layerstate(4)=0
				EndIf
			EndIf
		Case 42
			If layer_active=4 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4b), TRUE)
				layerstate(4)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb4b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb4a))
					If toggle_state=TRUE Then
						layerstate(4)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4b), FALSE)
						layerstate(4)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb4a))
					If toggle_state=TRUE Then
						layerstate(4)=1
					Else
						layerstate(4)=0
					EndIf
				EndIf
			EndIf


		Case 51
			If layer_active=5 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5b), TRUE)
				layerstate(5)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb5a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb5b))
					If toggle_state=TRUE Then
						layerstate(5)=2
					Else
						layerstate(5)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5b), FALSE)
					layerstate(5)=0
				EndIf
			EndIf
		Case 52
			If layer_active=5 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5b), TRUE)
				layerstate(5)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb5b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb5a))
					If toggle_state=TRUE Then
						layerstate(5)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5b), FALSE)
						layerstate(5)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb5a))
					If toggle_state=TRUE Then
						layerstate(5)=1
					Else
						layerstate(5)=0
					EndIf
				EndIf
			EndIf


		Case 61
			If layer_active=6 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6b), TRUE)
				layerstate(6)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb6a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb6b))
					If toggle_state=TRUE Then
						layerstate(6)=2
					Else
						layerstate(6)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6b), FALSE)
					layerstate(6)=0
				EndIf
			EndIf
		Case 62
			If layer_active=6 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6b), TRUE)
				layerstate(6)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb6b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb6a))
					If toggle_state=TRUE Then
						layerstate(6)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6b), FALSE)
						layerstate(6)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb6a))
					If toggle_state=TRUE Then
						layerstate(6)=1
					Else
						layerstate(6)=0
					EndIf
				EndIf
			EndIf



		Case 71
			If layer_active=7 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7b), TRUE)
				layerstate(7)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb7a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb7b))
					If toggle_state=TRUE Then
						layerstate(7)=2
					Else
						layerstate(7)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7b), FALSE)
					layerstate(7)=0
				EndIf
			EndIf
		Case 72
			If layer_active=7 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7b), TRUE)
				layerstate(7)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb7b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb7a))
					If toggle_state=TRUE Then
						layerstate(7)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7b), FALSE)
						layerstate(7)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb7a))
					If toggle_state=TRUE Then
						layerstate(7)=1
					Else
						layerstate(7)=0
					EndIf
				EndIf
			EndIf



		Case 81
			If layer_active=8 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8b), TRUE)
				layerstate(8)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb8a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb8b))
					If toggle_state=TRUE Then
						layerstate(8)=2
					Else
						layerstate(8)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8b), FALSE)
					layerstate(8)=0
				EndIf
			EndIf
		Case 82
			If layer_active=8 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8b), TRUE)
				layerstate(8)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb8b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb8a))
					If toggle_state=TRUE Then
						layerstate(8)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8b), FALSE)
						layerstate(8)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb8a))
					If toggle_state=TRUE Then
						layerstate(8)=1
					Else
						layerstate(8)=0
					EndIf
				EndIf
			EndIf



		Case 91
			If layer_active=9 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9b), TRUE)
				layerstate(9)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb9a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb9b))
					If toggle_state=TRUE Then
						layerstate(9)=2
					Else
						layerstate(9)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9b), FALSE)
					layerstate(9)=0
				EndIf
			EndIf
		Case 92
			If layer_active=9 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9b), TRUE)
				layerstate(9)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb9b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb9a))
					If toggle_state=TRUE Then
						layerstate(9)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9b), FALSE)
						layerstate(9)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb9a))
					If toggle_state=TRUE Then
						layerstate(9)=1
					Else
						layerstate(9)=0
					EndIf
				EndIf
			EndIf



		Case 101
			If layer_active=10 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10b), TRUE)
				layerstate(10)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb10a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb10b))
					If toggle_state=TRUE Then
						layerstate(10)=2
					Else
						layerstate(10)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10b), FALSE)
					layerstate(10)=0
				EndIf
			EndIf
		Case 102
			If layer_active=10 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10b), TRUE)
				layerstate(10)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb10b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb10a))
					If toggle_state=TRUE Then
						layerstate(10)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10b), FALSE)
						layerstate(10)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb10a))
					If toggle_state=TRUE Then
						layerstate(10)=1
					Else
						layerstate(10)=0
					EndIf
				EndIf
			EndIf


		Case 111
			If layer_active=11 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11b), TRUE)
				layerstate(11)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb11a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb11b))
					If toggle_state=TRUE Then
						layerstate(11)=2
					Else
						layerstate(11)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11b), FALSE)
					layerstate(11)=0
				EndIf
			EndIf
		Case 112
			If layer_active=11 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11b), TRUE)
				layerstate(11)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb11b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb11a))
					If toggle_state=TRUE Then
						layerstate(11)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11b), FALSE)
						layerstate(11)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb11a))
					If toggle_state=TRUE Then
						layerstate(11)=1
					Else
						layerstate(11)=0
					EndIf
				EndIf
			EndIf


		Case 121
			If layer_active=12 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12b), TRUE)
				layerstate(12)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb12a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb12b))
					If toggle_state=TRUE Then
						layerstate(12)=2
					Else
						layerstate(12)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12b), FALSE)
					layerstate(12)=0
				EndIf
			EndIf
		Case 122
			If layer_active=12 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12b), TRUE)
				layerstate(12)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb12b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb12a))
					If toggle_state=TRUE Then
						layerstate(12)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12b), FALSE)
						layerstate(12)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb12a))
					If toggle_state=TRUE Then
						layerstate(12)=1
					Else
						layerstate(12)=0
					EndIf
				EndIf
			EndIf


		Case 131
			If layer_active=13 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13b), TRUE)
				layerstate(13)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb13a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb13b))
					If toggle_state=TRUE Then
						layerstate(13)=2
					Else
						layerstate(13)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13b), FALSE)
					layerstate(13)=0
				EndIf
			EndIf
		Case 132
			If layer_active=13 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13b), TRUE)
				layerstate(13)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb13b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb13a))
					If toggle_state=TRUE Then
						layerstate(13)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13b), FALSE)
						layerstate(13)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb13a))
					If toggle_state=TRUE Then
						layerstate(13)=1
					Else
						layerstate(13)=0
					EndIf
				EndIf
			EndIf


		Case 141
			If layer_active=14 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14b), TRUE)
				layerstate(14)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb14a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb14b))
					If toggle_state=TRUE Then
						layerstate(14)=2
					Else
						layerstate(14)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14b), FALSE)
					layerstate(14)=0
				EndIf
			EndIf
		Case 142
			If layer_active=14 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14b), TRUE)
				layerstate(14)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb14b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb14a))
					If toggle_state=TRUE Then
						layerstate(14)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14b), FALSE)
						layerstate(14)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb14a))
					If toggle_state=TRUE Then
						layerstate(14)=1
					Else
						layerstate(14)=0
					EndIf
				EndIf
			EndIf


		Case 151
			If layer_active=15 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15b), TRUE)
				layerstate(15)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb15a))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb15b))
					If toggle_state=TRUE Then
						layerstate(15)=2
					Else
						layerstate(15)=1
					EndIf
				Else
					gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15b), FALSE)
					layerstate(15)=0
				EndIf
			EndIf
		Case 152
			If layer_active=15 Then
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15a), TRUE)
				gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15b), TRUE)
				layerstate(15)=2
			Else
				toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb15b))
				If toggle_state=TRUE Then
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb15a))
					If toggle_state=TRUE Then
						layerstate(15)=2
					Else
						gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15b), FALSE)
						layerstate(15)=0
					EndIf
				Else
					toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(layergcb15a))
					If toggle_state=TRUE Then
						layerstate(15)=1
					Else
						layerstate(15)=0
					EndIf
				EndIf
			EndIf



	End Select
	inview
	redraw
	layerstatebuttonclicked=FALSE
End Sub
SUB layer_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	If layerbuttonclicked=TRUE Then Exit Sub
	layerbuttonclicked=TRUE
	Dim i As Integer
	i=CAST(INTEGER, dat)
	g_object_ref(mylayerimageup1)
	gtk_button_set_image(layergb1, mylayerimageup1)
	g_object_ref(mylayerimageup2)
	gtk_button_set_image(layergb2, mylayerimageup2)
	g_object_ref(mylayerimageup3)
	gtk_button_set_image(layergb3, mylayerimageup3)
	g_object_ref(mylayerimageup4)
	gtk_button_set_image(layergb4, mylayerimageup4)
	g_object_ref(mylayerimageup5)
	gtk_button_set_image(layergb5, mylayerimageup5)
	g_object_ref(mylayerimageup6)
	gtk_button_set_image(layergb6, mylayerimageup6)
	g_object_ref(mylayerimageup7)
	gtk_button_set_image(layergb7, mylayerimageup7)
	g_object_ref(mylayerimageup8)
	gtk_button_set_image(layergb8, mylayerimageup8)
	g_object_ref(mylayerimageup9)
	gtk_button_set_image(layergb9, mylayerimageup9)
	g_object_ref(mylayerimageup10)
	gtk_button_set_image(layergb10, mylayerimageup10)
	g_object_ref(mylayerimageup11)
	gtk_button_set_image(layergb11, mylayerimageup11)
	g_object_ref(mylayerimageup12)
	gtk_button_set_image(layergb12, mylayerimageup12)
	g_object_ref(mylayerimageup13)
	gtk_button_set_image(layergb13, mylayerimageup13)
	g_object_ref(mylayerimageup14)
	gtk_button_set_image(layergb14, mylayerimageup14)
	g_object_ref(mylayerimageup15)
	gtk_button_set_image(layergb15, mylayerimageup15)
	Select Case i
		Case 1
			mytextbuffer="Layer1 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn1)
			gtk_button_set_image(layergb1, mylayerimagedn1)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb1b), TRUE)
			layer_active=1
		Case 2
			mytextbuffer="Layer2 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn2)
			gtk_button_set_image(layergb2, mylayerimagedn2)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb2b), TRUE)
			layer_active=2
		Case 3
			mytextbuffer="Layer3 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn3)
			gtk_button_set_image(layergb3, mylayerimagedn3)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb3b), TRUE)
			layer_active=3
		Case 4
			mytextbuffer="Layer4 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn4)
			gtk_button_set_image(layergb4, mylayerimagedn4)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb4b), TRUE)
			layer_active=4
		Case 5
			mytextbuffer="Layer5 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn5)
			gtk_button_set_image(layergb5, mylayerimagedn5)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb5b), TRUE)
			layer_active=5
		Case 6
			mytextbuffer="Layer6 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn6)
			gtk_button_set_image(layergb6, mylayerimagedn6)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb6b), TRUE)
			layer_active=6
		Case 7
			mytextbuffer="Layer7 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn7)
			gtk_button_set_image(layergb7, mylayerimagedn7)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb7b), TRUE)
			layer_active=7
		Case 8
			mytextbuffer="Layer8 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn8)
			gtk_button_set_image(layergb8, mylayerimagedn8)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb8b), TRUE)
			layer_active=8
		Case 9
			mytextbuffer="Layer9 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn9)
			gtk_button_set_image(layergb9, mylayerimagedn9)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb9b), TRUE)
			layer_active=9
		Case 10
			mytextbuffer="Layer10 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn10)
			gtk_button_set_image(layergb10, mylayerimagedn10)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb10b), TRUE)
			layer_active=10
		Case 11
			mytextbuffer="Layer11 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn11)
			gtk_button_set_image(layergb11, mylayerimagedn11)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb11b), TRUE)
			layer_active=11
		Case 12
			mytextbuffer="Layer12 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn12)
			gtk_button_set_image(layergb12, mylayerimagedn12)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb12b), TRUE)
			layer_active=12
		Case 13
			mytextbuffer="Layer13 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn13)
			gtk_button_set_image(layergb13, mylayerimagedn13)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb13b), TRUE)
			layer_active=13
		Case 14
			mytextbuffer="Layer14 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn14)
			gtk_button_set_image(layergb14, mylayerimagedn14)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb14b), TRUE)
			layer_active=14
		Case 15
			mytextbuffer="Layer15 set active"+Chr(10)+mytextbuffer
			g_object_ref(mylayerimagedn15)
			gtk_button_set_image(layergb15, mylayerimagedn15)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15a), TRUE)
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(layergcb15b), TRUE)
			layer_active=15
	End Select
	layerstate(layer_active)=2
	lc=layer_active
	For i = 1 To linec
		If lines(i,8)=1 Then lines(i,7)=lc
	Next
	For i = 1 To circlec
		If circles(i,10)=1 Then circles(i,5)=lc
	Next
	escapeme	
	
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	layerbuttonclicked=FALSE
END Sub
Sub button_enter Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim i As Integer
	i=CAST(INTEGER, dat)
	Select Case i
		Case 0
			gtk_text_buffer_set_text (textinfobuffer, fbcad_desc, -1)
		Case 11
			gtk_text_buffer_set_text (textinfobuffer, bae1gd1, -1)
		Case 12
			gtk_text_buffer_set_text (textinfobuffer, bae1gd2, -1)
		Case 13
			gtk_text_buffer_set_text (textinfobuffer, bae1gd3, -1)
		Case 14
			gtk_text_buffer_set_text (textinfobuffer, bae1gd4, -1)
		Case 15
			gtk_text_buffer_set_text (textinfobuffer, bae1gd5, -1)
		Case 16
			gtk_text_buffer_set_text (textinfobuffer, bae1gd6, -1)
		Case 21
			gtk_text_buffer_set_text (textinfobuffer, bae2gd1, -1)
		Case 22
			gtk_text_buffer_set_text (textinfobuffer, bae2gd2, -1)
		Case 23
			gtk_text_buffer_set_text (textinfobuffer, bae2gd3, -1)
		Case 24
			gtk_text_buffer_set_text (textinfobuffer, bae2gd4, -1)
		Case 25
			gtk_text_buffer_set_text (textinfobuffer, bae2gd5, -1)
		Case 26
			gtk_text_buffer_set_text (textinfobuffer, bae2gd6, -1)
		Case 27
			gtk_text_buffer_set_text (textinfobuffer, bae2gd7, -1)
		Case 28
			gtk_text_buffer_set_text (textinfobuffer, bae2gd8, -1)
		Case 29
			gtk_text_buffer_set_text (textinfobuffer, bae2gd9, -1)
		Case 210
			gtk_text_buffer_set_text (textinfobuffer, bae2gd10, -1)
		Case 31
			gtk_text_buffer_set_text (textinfobuffer, bae3gd1, -1)
		Case 32
			gtk_text_buffer_set_text (textinfobuffer, bae3gd2, -1)
		Case 33
			gtk_text_buffer_set_text (textinfobuffer, bae3gd3, -1)
		Case 33
			gtk_text_buffer_set_text (textinfobuffer, bae3gd3, -1)
		Case 34
			gtk_text_buffer_set_text (textinfobuffer, bae3gd4, -1)
		Case 35
			gtk_text_buffer_set_text (textinfobuffer, bae3gd5, -1)
		Case 36
			gtk_text_buffer_set_text (textinfobuffer, bae3gd6, -1)
		Case 37
			gtk_text_buffer_set_text (textinfobuffer, bae3gd7, -1)
		Case 38
			gtk_text_buffer_set_text (textinfobuffer, bae3gd8, -1)
		Case 41
			gtk_text_buffer_set_text (textinfobuffer, bae4gd1, -1)
		Case 42
			gtk_text_buffer_set_text (textinfobuffer, bae4gd2, -1)
		Case 43
			gtk_text_buffer_set_text (textinfobuffer, bae4gd3, -1)
		Case 44
			gtk_text_buffer_set_text (textinfobuffer, bae4gd4, -1)
		Case 45
			gtk_text_buffer_set_text (textinfobuffer, bae4gd5, -1)
		Case 46
			gtk_text_buffer_set_text (textinfobuffer, bae4gd6, -1)
		Case 47
			gtk_text_buffer_set_text (textinfobuffer, bae4gd7, -1)
		Case 48
			gtk_text_buffer_set_text (textinfobuffer, bae4gd8, -1)
		Case 49
			gtk_text_buffer_set_text (textinfobuffer, bae4gd9, -1)
		Case 410
			gtk_text_buffer_set_text (textinfobuffer, bae4gd10, -1)
		Case 51
			gtk_text_buffer_set_text (textinfobuffer, bae5gd1, -1)
		Case 52
			gtk_text_buffer_set_text (textinfobuffer, bae5gd2, -1)
		Case 53
			gtk_text_buffer_set_text (textinfobuffer, bae5gd3, -1)
		Case 54
			gtk_text_buffer_set_text (textinfobuffer, bae5gd4, -1)
		Case 55
			gtk_text_buffer_set_text (textinfobuffer, bae5gd5, -1)
		Case 56
			gtk_text_buffer_set_text (textinfobuffer, bae5gd6, -1)
		Case 57
			gtk_text_buffer_set_text (textinfobuffer, bae5gd7, -1)
		Case 58
			gtk_text_buffer_set_text (textinfobuffer, bae5gd8, -1)
		Case 59
			gtk_text_buffer_set_text (textinfobuffer, bae5gd9, -1)
		Case 510
			gtk_text_buffer_set_text (textinfobuffer, bae5gd10, -1)
		Case 61
			gtk_text_buffer_set_text (textinfobuffer, bae6gd1, -1)
		Case 62
			gtk_text_buffer_set_text (textinfobuffer, bae6gd2, -1)
		Case 63
			gtk_text_buffer_set_text (textinfobuffer, bae6gd3, -1)
		Case 64
			gtk_text_buffer_set_text (textinfobuffer, bae6gd4, -1)
		Case 65
			gtk_text_buffer_set_text (textinfobuffer, bae6gd5, -1)
		Case 66
			gtk_text_buffer_set_text (textinfobuffer, bae6gd6, -1)
		Case 67
			gtk_text_buffer_set_text (textinfobuffer, bae6gd7, -1)
		Case 68
			gtk_text_buffer_set_text (textinfobuffer, bae6gd8, -1)
		Case 69
			gtk_text_buffer_set_text (textinfobuffer, bae6gd9, -1)
		Case 610
			gtk_text_buffer_set_text (textinfobuffer, bae6gd10, -1)
		Case 71
			gtk_text_buffer_set_text (textinfobuffer, bae7gd1, -1)
		Case 72
			gtk_text_buffer_set_text (textinfobuffer, bae7gd2, -1)
		Case 73
			gtk_text_buffer_set_text (textinfobuffer, bae7gd3, -1)
		Case 74
			gtk_text_buffer_set_text (textinfobuffer, bae7gd4, -1)
		Case 75
			gtk_text_buffer_set_text (textinfobuffer, bae7gd5, -1)
		Case 76
			gtk_text_buffer_set_text (textinfobuffer, bae7gd6, -1)
		Case 77
			gtk_text_buffer_set_text (textinfobuffer, bae7gd7, -1)
		Case 81
			gtk_text_buffer_set_text (textinfobuffer, bae8gd1, -1)
		Case 82
			gtk_text_buffer_set_text (textinfobuffer, bae8gd2, -1)
		Case 83
			gtk_text_buffer_set_text (textinfobuffer, bae8gd3, -1)
		Case 84
			gtk_text_buffer_set_text (textinfobuffer, bae8gd4, -1)
		Case 85
			gtk_text_buffer_set_text (textinfobuffer, bae8gd5, -1)
		Case 86
			gtk_text_buffer_set_text (textinfobuffer, bae8gd6, -1)
		Case 87
			gtk_text_buffer_set_text (textinfobuffer, bae8gd7, -1)
		Case 88
			gtk_text_buffer_set_text (textinfobuffer, bae8gd8, -1)
		Case 89
			gtk_text_buffer_set_text (textinfobuffer, bae8gd9, -1)
		Case 810
			gtk_text_buffer_set_text (textinfobuffer, bae8gd10, -1)
		Case 91
			gtk_text_buffer_set_text (textinfobuffer, bae9gd1, -1)
		Case 92
			gtk_text_buffer_set_text (textinfobuffer, bae9gd2, -1)
		Case 93
			gtk_text_buffer_set_text (textinfobuffer, bae9gd3, -1)
		Case 94
			gtk_text_buffer_set_text (textinfobuffer, bae9gd4, -1)
		Case 101
			gtk_text_buffer_set_text (textinfobuffer, bae10gd1, -1)
		Case 102
			gtk_text_buffer_set_text (textinfobuffer, bae10gd2, -1)
		Case 103
			gtk_text_buffer_set_text (textinfobuffer, bae10gd3, -1)
		Case 104
			gtk_text_buffer_set_text (textinfobuffer, bae10gd4, -1)
		Case 111
			gtk_text_buffer_set_text (textinfobuffer, bae11gd1, -1)
		Case 112
			gtk_text_buffer_set_text (textinfobuffer, bae11gd2, -1)
		Case 113
			gtk_text_buffer_set_text (textinfobuffer, bae11gd3, -1)
		Case 121
			gtk_text_buffer_set_text (textinfobuffer, bae12gd1, -1)
		Case 122
			gtk_text_buffer_set_text (textinfobuffer, bae12gd2, -1)
		Case 123
			gtk_text_buffer_set_text (textinfobuffer, bae12gd3, -1)
		Case 124
			gtk_text_buffer_set_text (textinfobuffer, bae12gd4, -1)
		Case 131
			gtk_text_buffer_set_text (textinfobuffer, bae13gd1, -1)
		Case 132
			gtk_text_buffer_set_text (textinfobuffer, bae13gd2, -1)
		Case 133
			gtk_text_buffer_set_text (textinfobuffer, bae13gd3, -1)
		Case 134
			gtk_text_buffer_set_text (textinfobuffer, bae13gd4, -1)
		Case 141
			gtk_text_buffer_set_text (textinfobuffer, bae14gd1, -1)
		Case 142
			gtk_text_buffer_set_text (textinfobuffer, bae14gd2, -1)
		Case 143
			gtk_text_buffer_set_text (textinfobuffer, bae14gd3, -1)
		Case 144
			gtk_text_buffer_set_text (textinfobuffer, bae14gd4, -1)
		Case 145
			gtk_text_buffer_set_text (textinfobuffer, bae14gd5, -1)
		Case 146
			gtk_text_buffer_set_text (textinfobuffer, bae14gd6, -1)
		Case 147
			gtk_text_buffer_set_text (textinfobuffer, bae14gd7, -1)
		Case 148
			gtk_text_buffer_set_text (textinfobuffer, bae14gd8, -1)
		Case 149
			gtk_text_buffer_set_text (textinfobuffer, bae14gd9, -1)
		Case 151
			gtk_text_buffer_set_text (textinfobuffer, bae15gd1, -1)
		Case 152
			gtk_text_buffer_set_text (textinfobuffer, bae15gd2, -1)
		Case 153
			gtk_text_buffer_set_text (textinfobuffer, bae15gd3, -1)
		Case 154
			gtk_text_buffer_set_text (textinfobuffer, bae15gd4, -1)
		Case 155
			gtk_text_buffer_set_text (textinfobuffer, bae15gd5, -1)
		Case 156
			gtk_text_buffer_set_text (textinfobuffer, bae15gd6, -1)
		Case 157
			gtk_text_buffer_set_text (textinfobuffer, bae15gd7, -1)
	End Select
End Sub
Sub pae2gsb_enter Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	pae2gsb_active=TRUE
End Sub
Sub pae2gsb_leave Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	pae2gsb_active=FALSE
End Sub
Sub pae2gsbadj_value_changed Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim i As Integer
	If pae2gsb_active=TRUE Then
		i = Int(gtk_adjustment_get_value(pae2gsbadj))
		i+=1
		gtk_properties_line_i=inviewfilterlines(i)
		i=gtk_properties_line_i
		degroup
		selcircle=0
		selline=i
		group
	Else
		i=CAST(Integer, dat)
		If i<>-1 Then
			gtk_adjustment_set_value(pae2gsbadj,i-1)
			i = Int(gtk_adjustment_get_value(pae2gsbadj))
			i+=1
			gtk_properties_line_i=inviewfilterlines(i)
			i=gtk_properties_line_i
		Else
			i = Int(gtk_adjustment_get_value(pae2gsbadj))
			i+=1
			gtk_properties_line_i=inviewfilterlines(i)
			i=gtk_properties_line_i
		EndIf
	EndIf
	xlength=lines(i,4)-lines(i,1)
	ylength=lines(i,5)-lines(i,2)
	gtk_properties_line_angle=atan2(ylength,xlength)*r2d
	gtk_properties_line_angle+=360
	gtk_properties_line_angle=mymod(gtk_properties_line_angle,360)
	gtk_properties_line_length = sqr((lines(i,1)-lines(i,4))^2 + (lines(i,2)-lines(i,5))^2 + (lines(i,3)-lines(i,6))^2)
	gtk_entry_set_text(GTK_ENTRY(pae2ge1),Str(lines(i,1)))
	gtk_entry_set_text(GTK_ENTRY(pae2ge2),Str(lines(i,2)))
	gtk_entry_set_text(GTK_ENTRY(pae2ge3),Str(lines(i,4)))
	gtk_entry_set_text(GTK_ENTRY(pae2ge4),Str(lines(i,5)))
	gtk_entry_set_text(GTK_ENTRY(pae2ge5),Str(gtk_properties_line_angle))
	gtk_entry_set_text(GTK_ENTRY(pae2ge6),Str(gtk_properties_line_length))
	gtk_expander_set_label (paexpander2,"Line Properties - Line #"+Str(i-preloadedlinec))
	gtk_widget_grab_focus(ebox)
End Sub
Sub pae2gsbadj_config(value As Integer,upper As Integer)
	gtk_adjustment_configure(pae2gsbadj,value,0,upper,1,0,1)
End Sub
Function pae2ge_click Cdecl(Byval widget As GtkWidget Ptr, byval event as GdkEventButton ptr, Byval dat As gpointer) As gboolean
	Dim As Integer i,dati
	dati=CAST(Integer, dat)
	If linesingroupc<>1 Then
		i = Int(gtk_adjustment_get_value(pae2gsbadj))
		i+=1
		gtk_properties_line_i=inviewfilterlines(i)
		i=gtk_properties_line_i
		degroup
		selcircle=0
		selline=i
		group
	EndIf
	Return FALSE
End Function
Sub pae2gb_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim As BOOLEAN attributes_changed=FALSE
	Dim As Integer i
	Dim As BOOLEAN valid
	valid=TRUE
	i=selline
	If selline=0 Then Exit Sub
	If *gtk_entry_get_text(pae2ge1)<>Str(lines(i,1)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae2ge2)<>Str(lines(i,2)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae2ge3)<>Str(lines(i,4)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae2ge4)<>Str(lines(i,5)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae2ge5)<>Str(gtk_properties_line_angle) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae2ge6)<>Str(gtk_properties_line_length) Then attributes_changed=TRUE
	If attributes_changed=FALSE Then Exit Sub
	If Str(gtk_properties_line_angle) <> *gtk_entry_get_text(pae2ge5) Or Str(gtk_properties_line_length) <> *gtk_entry_get_text(pae2ge6) Then'angle or length update
		If Str(lines(i,4)) <> *gtk_entry_get_text(pae2ge3) Then valid=FALSE
		If Str(lines(i,5)) <> *gtk_entry_get_text(pae2ge4) Then valid=FALSE
		If valid=TRUE Then
			x1=Val(*gtk_entry_get_text(pae2ge1))
			y1=Val(*gtk_entry_get_text(pae2ge2))
			selangle = Val(*gtk_entry_get_text(pae2ge5))
			sellength = Val(*gtk_entry_get_text(pae2ge6))
			altosellength
			lines(i,1)=x1
			lines(i,2)=y1
			lines(i,4)=fx
			lines(i,5)=fy
		EndIf
	Else'points update
		lines(i,1)=Val(*gtk_entry_get_text(pae2ge1))
		lines(i,2)=Val(*gtk_entry_get_text(pae2ge2))
		lines(i,4)=Val(*gtk_entry_get_text(pae2ge3))
		lines(i,5)=Val(*gtk_entry_get_text(pae2ge4))
	EndIf
	theboxbelow("line #"+Str(i-preloadedlinec)+" attributes updated")
	i = Int(gtk_adjustment_get_value(pae2gsbadj))
	i+=1
	pae2gsbadj_value_changed(NULL, Cast(gpointer, i))
	degroup
	inview
	redraw
	gtk_widget_grab_focus(ebox)
End Sub
Sub pae3gsb_enter Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	pae3gsb_active=TRUE
End Sub
Sub pae3gsb_leave Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	pae3gsb_active=FALSE
End Sub
Sub pae3gsbadj_value_changed Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim i As Integer
	If pae3gsb_active=TRUE Then
		i = Int(gtk_adjustment_get_value(pae3gsbadj))
		i+=1
		gtk_properties_circle_i=inviewfiltercircles(i)
		i=gtk_properties_circle_i
		degroup
		selcircle=i
		selline=0
		group
	Else
		i=CAST(Integer, dat)
		If i<>-1 Then
			gtk_adjustment_set_value(pae3gsbadj,i-1)
			i = Int(gtk_adjustment_get_value(pae3gsbadj))
			i+=1
			gtk_properties_circle_i=inviewfiltercircles(i)
			i=gtk_properties_circle_i
		Else
			i = Int(gtk_adjustment_get_value(pae3gsbadj))
			i+=1
			gtk_properties_circle_i=inviewfiltercircles(i)
			i=gtk_properties_circle_i
		EndIf
	EndIf
	gtk_entry_set_text(GTK_ENTRY(pae3ge1),Str(circles(i,1)))
	gtk_entry_set_text(GTK_ENTRY(pae3ge2),Str(circles(i,2)))
	gtk_entry_set_text(GTK_ENTRY(pae3ge3),Str(circles(i,4)))
	gtk_entry_set_text(GTK_ENTRY(pae3ge4),Str(circles(i,8)))
	Select Case circles(i,9)
		Case 1,2
			gtk_entry_set_text(GTK_ENTRY(pae3ge5),Str(circles(i,6)*180/pi))
			gtk_entry_set_text(GTK_ENTRY(pae3ge6),Str(circles(i,7)*180/pi))
		Case 3,4
			gtk_entry_set_text(GTK_ENTRY(pae3ge5),Str(circles(i,6)))
			gtk_entry_set_text(GTK_ENTRY(pae3ge6),Str(circles(i,7)))
	End Select
	gtk_entry_set_text(GTK_ENTRY(pae3ge7),Str(circles(i,11)))
	gtk_expander_set_label (paexpander3,"Circle Properties - Circle #"+Str(i-preloadedcirclec))
	gtk_widget_grab_focus(ebox)
End Sub
Sub pae3gsbadj_config(value As Integer,upper As Integer)
	gtk_adjustment_configure(pae3gsbadj,value,0,upper,1,0,1)
End Sub
Function pae3ge_click Cdecl(Byval widget As GtkWidget Ptr, byval event as GdkEventButton ptr, Byval dat As gpointer) As gboolean
	Dim As Integer i,dati
	dati=CAST(Integer, dat)
		If circlesingroupc<>1 Then
			i = Int(gtk_adjustment_get_value(pae3gsbadj))
			i+=1
			gtk_properties_line_i=inviewfiltercircles(i)
			i=gtk_properties_circle_i
			degroup
			selcircle=i
			selline=0
			group
		EndIf
		Return FALSE
End Function
Sub pae3gb_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim As BOOLEAN attributes_changed=FALSE
	Dim As Integer i
	i=selcircle
	If selcircle=0 Then Exit Sub
	If *gtk_entry_get_text(pae3ge1)<>Str(circles(i,1)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae3ge2)<>Str(circles(i,2)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae3ge3)<>Str(circles(i,4)) Then attributes_changed=TRUE
	If *gtk_entry_get_text(pae3ge4)<>Str(circles(i,8)) Then attributes_changed=TRUE
	Select Case circles(i,9)
		Case 1,2
			If *gtk_entry_get_text(pae3ge5)<>Str(circles(i,6)*180/pi) Then attributes_changed=TRUE
			If *gtk_entry_get_text(pae3ge6)<>Str(circles(i,7)*180/pi) Then attributes_changed=TRUE
		Case 3,4
			If *gtk_entry_get_text(pae3ge5)<>Str(circles(i,6)) Then attributes_changed=TRUE
			If *gtk_entry_get_text(pae3ge6)<>Str(circles(i,7)) Then attributes_changed=TRUE
	End Select
	If attributes_changed=FALSE Then Exit Sub
	
	If Val(*gtk_entry_get_text(pae3ge4))=0 Then
		If Val(*gtk_entry_get_text(pae3ge5)) = 0 And Val(*gtk_entry_get_text(pae3ge6)) = 0 Or Val(*gtk_entry_get_text(pae3ge5)) = 0 And Val(*gtk_entry_get_text(pae3ge6)) = 360 Then
			circles(i,9)=1
			circles(i,6)=0
			circles(i,7)=0
		Else
			circles(i,9)=2
			'radians=degrees*d2r
			circles(i,6)=mymod(Val(*gtk_entry_get_text(pae3ge5)),360)*d2r
			circles(i,7)=mymod(Val(*gtk_entry_get_text(pae3ge6)),360)*d2r
			If circles(i,6)=360 Then circles(i,6)=0
			If circles(i,7)=360 Then circles(i,7)=0
		EndIf
		circles(i,11)=0
	Else
		If Val(*gtk_entry_get_text(pae3ge5)) = 0 And Val(*gtk_entry_get_text(pae3ge6)) = 360 Or Val(*gtk_entry_get_text(pae3ge5)) = 0 And Val(*gtk_entry_get_text(pae3ge6)) = 0 Then
			circles(i,9)=3
			circles(i,6)=0
			circles(i,7)=360
		Else
			circles(i,9)=4
			circles(i,6)=mymod(Val(*gtk_entry_get_text(pae3ge5)),360)
			circles(i,7)=mymod(Val(*gtk_entry_get_text(pae3ge6)),360)
		EndIf
		circles(i,11)=Val(*gtk_entry_get_text(pae3ge7))
	EndIf
	theboxbelow("circle #"+Str(i-preloadedcirclec)+" attributes updated")
	circles(i,1)=Val(*gtk_entry_get_text(pae3ge1))
	circles(i,2)=Val(*gtk_entry_get_text(pae3ge2))
	circles(i,4)=Val(*gtk_entry_get_text(pae3ge3))
	circles(i,8)=Val(*gtk_entry_get_text(pae3ge4))
	pae3gsbadj_value_changed(NULL, Cast(gpointer, i))
	degroup
	inview
	redraw
	gtk_widget_grab_focus(ebox)
End Sub
SUB filtertype_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
	Dim i As Integer
	Dim As BOOLEAN toggle_state
	i=CAST(INTEGER, dat)

	'filtertype(1,1)="Y"'use filters
	'filtertype(1,2)="Use filters"
	'filtertype(2,1)="N"'filter all blocks
	'filtertype(2,2)="All blocks"'filter all blocks
	'filtertype(3,1)="Y"'filter text blocks
	'filtertype(3,2)="Text blocks"'filter text blocks
	''filtertyp(4-n)=(y/n) filter by block name

	Select Case i
		Case 1
			toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pae4gcb1))
			If toggle_state=TRUE Then
				filtertype(1,1)="Y"'use filters
			Else
				filtertype(1,1)="N"'dont use filters - just detect everything
			EndIf
		Case 2
			toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pae4gcb2))
			If toggle_state=TRUE Then
				filtertype(2,1)="Y"'filter all blocks
			Else
				filtertype(2,1)="N"'dont filter all blocks
			EndIf
		Case 3
			toggle_state=gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(pae4gcb3))
			If toggle_state=TRUE Then
				filtertype(3,1)="Y"'filter text blocks
			Else
				filtertype(3,1)="N"'dont filter text blocks
			EndIf
	End Select
	degroup
	inview
	redraw
	gtk_widget_grab_focus(ebox)
End Sub
Sub load_button_desc()
bae1gd1 = "draw lines: 2 left mouse clicks are used to draw a line. 1st click defines where the line starts and 2nd click defines where the line ends. Additional lines may be drawn and their end points are defined by subsequent left mouse clicks. Press the ESC key or right mouse click to stop drawing lines."
fbcad_desc+=Chr(10)+bae1gd1+Chr(10)
bae1gd2 = "draw circles: A circle can be drawn by defining its radius or diameter. 2 left mouse clicks are used to draw a circle. The 1st click difines the center point of the circle and the 2nd click defines its radius. By holding the shift key down, the 1st and 2nd clicks define its diameter."
fbcad_desc+=Chr(10)+bae1gd2+Chr(10)
bae1gd3 = "draw arcs: Arcs take 4 left mouse clicks to create. Similar to drawing a circle the 1st and 2nd clicks define the arc's radius and the 3rd and 4th clicks define the arc start and arc end. Arcs are drawn counter clockwise."
fbcad_desc+=Chr(10)+bae1gd3+Chr(10)
bae1gd4 = "draw ellipses: An ellipse has a major and minor radius. It takes 3 left mouse clicks to draw an ellipse. 1st click defines the center point. 2nd and 3rd clicks define the major and minor radii. Note: The 1st and 2nd clicks also define the rotation of the ellipse."
fbcad_desc+=Chr(10)+bae1gd4+Chr(10)
bae1gd5 = "draw elliptical arcs: Same as drwaing an ellipse but with 2 extra left mouse clicks to define the arc start and arc end."
fbcad_desc+=Chr(10)+bae1gd5+Chr(10)
bae1gd6 = "draw splines: A series of smoothly connected elliptical arcs which can create shapes such as that of an egg. 4 left mouse clicks start the spline. 5th, 6th and subsequent left mouse clicks continue the spline. Right mouse click ends the spline. Egg example: click around the 3 points of an isosceles triangle 6 times."
fbcad_desc+=Chr(10)+bae1gd6+Chr(10)
bae2gd1 = "Equilateral Triangles: code and description pending"
fbcad_desc+=Chr(10)+bae2gd1+Chr(10)
bae2gd2 = "Right Triangles: code and description pending"
fbcad_desc+=Chr(10)+bae2gd2+Chr(10)
bae2gd3 = "Squares: code and description pending"
fbcad_desc+=Chr(10)+bae2gd3+Chr(10)
bae2gd4 = "Rectangles: code and description pending"
fbcad_desc+=Chr(10)+bae2gd4+Chr(10)
bae2gd5 = "Rhombus: code and description pending"
fbcad_desc+=Chr(10)+bae2gd5+Chr(10)
bae2gd6 = "Parallelograms: code and description pending"
fbcad_desc+=Chr(10)+bae2gd6+Chr(10)
bae2gd7 = "Pentagons: code and description pending"
fbcad_desc+=Chr(10)+bae2gd7+Chr(10)
bae2gd8 = "Hexagons: code and description pending"
fbcad_desc+=Chr(10)+bae2gd8+Chr(10)
bae2gd9 = "Heptagons: code and description pending"
fbcad_desc+=Chr(10)+bae2gd9+Chr(10)
bae2gd10 = "Octagons: code and description pending"
fbcad_desc+=Chr(10)+bae2gd10+Chr(10)
bae3gd1 = "snap to end point: with snap on, you can connect lines to the end points of other lines. turn snap off in order to avoid connecting to end points of other lines."
fbcad_desc+=Chr(10)+bae3gd1+Chr(10)
bae3gd2 = "snap to mid point: turn mid point on to detect and snap to the mid point of a line."
fbcad_desc+=Chr(10)+bae3gd2+Chr(10)
bae3gd3 = "snap to perpendicular: turn perpendicular on to detect and snap to points that are perpedicular to other lines."
fbcad_desc+=Chr(10)+bae3gd3+Chr(10)
bae3gd4 = "snap to tangent: while drawing a line, you can snap to the tangent of a circle, arc, ellipse or elliptical arc"
fbcad_desc+=Chr(10)+bae3gd4+Chr(10)
bae3gd5 = "snap to center: snaps to the center of arcs, ellipses, and circles"
fbcad_desc+=Chr(10)+bae3gd5+Chr(10)
bae3gd6 = "Snap to Intersection: Snap to the intersecton of lines"
fbcad_desc+=Chr(10)+bae3gd6+Chr(10)
bae3gd7 = "Snap to Nearest Point: Snap to the nearest point of another object"
fbcad_desc+=Chr(10)+bae3gd7+Chr(10)
bae3gd8 = "Snap to arcs intercept: Snaps to the calculated intersection of an arc and other objects"
fbcad_desc+=Chr(10)+bae3gd8+Chr(10)
bae4gd1 = "Ortho Mode: Enable ortho mode to draw entities or move selected objects orthogonally. Ortho Mode can be used in combination with Use Angle, Perpendicular From and Tangent From."
fbcad_desc+=Chr(10)+bae4gd1+Chr(10)
bae4gd2 = "Use Angle: To draw entities or move selected objects at a specific angle, enable Use Angle. If one of the five preset angles are not selected you will be prompted for an angle to use. You can copy and paste the angles of existing objects in the drawing or manually enter desired angles to use."
fbcad_desc+=Chr(10)+bae4gd2+Chr(10)
bae4gd3 = "Set user angles: 5 user angles are available to set."
fbcad_desc+=Chr(10)+bae4gd3+Chr(10)
bae4gd4 = "User Preset angle1: With Use Angle enabled AND 1 of the 5 User Preset angles enabled, lines are drawn or objects are moved at specified angle."
fbcad_desc+=Chr(10)+bae4gd4+Chr(10)
bae4gd5 = "User Preset angle2: With Use Angle enabled AND 1 of the 5 User Preset angles enabled, lines are drawn or objects are moved at specified angle."
fbcad_desc+=Chr(10)+bae4gd5+Chr(10)
bae4gd6 = "User Preset angle3: With Use Angle enabled AND 1 of the 5 User Preset angles enabled, lines are drawn or objects are moved at specified angle."
fbcad_desc+=Chr(10)+bae4gd6+Chr(10)
bae4gd7 = "User Preset angle4: With Use Angle enabled AND 1 of the 5 User Preset angles enabled, lines are drawn or objects are moved at specified angle."
fbcad_desc+=Chr(10)+bae4gd7+Chr(10)
bae4gd8 = "User Preset angle5: With Use Angle enabled AND 1 of the 5 User Preset angles enabled, lines are drawn or objects are moved at specified angle."
fbcad_desc+=Chr(10)+bae4gd8+Chr(10)
bae4gd9 = "Perpendicular From: Enable Perpendicular From to draw entities or move selected objects perpendicular from the selected line or normal of an ellipse or elliptical arc."
fbcad_desc+=Chr(10)+bae4gd9+Chr(10)
bae4gd10 = "Tangent From: Enable Tangent From to draw entities or move selected objects tangent from the selected circle, arc, ellipse or elliptical arc."
fbcad_desc+=Chr(10)+bae4gd10+Chr(10)
bae5gd1 = "move: move selected group from point to point"
fbcad_desc+=Chr(10)+bae5gd1+Chr(10)
bae5gd2 = "copy: coppies and moves selected group from point to point"
fbcad_desc+=Chr(10)+bae5gd2+Chr(10)
bae5gd3 = "rotate: rotates selected group at pivot point to angle"
fbcad_desc+=Chr(10)+bae5gd3+Chr(10)
bae5gd4 = "copy-rotate: coppies and rotates selected group from pivot point to angle"
fbcad_desc+=Chr(10)+bae5gd4+Chr(10)
bae5gd5 = "flip vertical: flips selected group vertically at point"
fbcad_desc+=Chr(10)+bae5gd5+Chr(10)
bae5gd6 = "flip horizontal: flips selected group horizontally at point"
fbcad_desc+=Chr(10)+bae5gd6+Chr(10)
bae5gd7 = "copy flip vertical: coppies and flips selected group vertically from point to point"
fbcad_desc+=Chr(10)+bae5gd7+Chr(10)
bae5gd8 = "copy flip horizontal: coppies and flips selected group horizontally from point to point"
fbcad_desc+=Chr(10)+bae5gd8+Chr(10)
bae5gd9 = "flip v & h: flips selected group vertically & horizontally at point"
fbcad_desc+=Chr(10)+bae5gd9+Chr(10)
bae5gd10 = "copy flip v & h: coppies and flips selected group vertically & horizontally from point to point"
fbcad_desc+=Chr(10)+bae5gd10+Chr(10)
bae6gd1 = "zoom in: zoom in on the drawing - rotating the mouse wheel in the drawing area does the same"
fbcad_desc+=Chr(10)+bae6gd1+Chr(10)
bae6gd2 = "zoom out: zoom out on the drawing - rotating the mouse wheel in the drawing area does the same"
fbcad_desc+=Chr(10)+bae6gd2+Chr(10)
bae6gd3 = "pan: pan the drawing left,right,up,down,diagonally by holding the left mouse buttun down and dragging"
fbcad_desc+=Chr(10)+bae6gd3+Chr(10)
bae6gd4 = "view extents: zoom out to the extents of the drawing"
fbcad_desc+=Chr(10)+bae6gd4+Chr(10)
bae6gd5 = "Save view: Saves current view"
fbcad_desc+=Chr(10)+bae6gd5+Chr(10)
bae6gd6 = "Previous view: Changes veiw to previous saved view"
fbcad_desc+=Chr(10)+bae6gd6+Chr(10)
bae6gd7 = "Next view: Changes veiw to next saved veiw"
fbcad_desc+=Chr(10)+bae6gd7+Chr(10)
bae6gd8 = "First view: Changes veiw to first saved veiw"
fbcad_desc+=Chr(10)+bae6gd8+Chr(10)
bae6gd9 = "Last view: Changes veiw to last saved veiw"
fbcad_desc+=Chr(10)+bae6gd9+Chr(10)
bae6gd10 = "Delete view: Deletes current saved view if viewing a saved view"
fbcad_desc+=Chr(10)+bae6gd10+Chr(10)
bae7gd1 = "Dimension: Dimension of distance between two points"
fbcad_desc+=Chr(10)+bae7gd1+Chr(10)
bae7gd2 = "X Dimension: Dimension of distance on the X axis"
fbcad_desc+=Chr(10)+bae7gd2+Chr(10)
bae7gd3 = "Y Dimension: Dimension of distance on the Y axis"
fbcad_desc+=Chr(10)+bae7gd3+Chr(10)
bae7gd4 = "Radius Dimension: Dimension of radius for circles and arcs."
fbcad_desc+=Chr(10)+bae7gd4+Chr(10)
bae7gd5 = "Diameter Dimension: Dimension of diameter for circles."
fbcad_desc+=Chr(10)+bae7gd5+Chr(10)
bae7gd6 = "Angle Dimension: Dimension of an angle using the inside of the angle to show the amount of degrees of arc."
fbcad_desc+=Chr(10)+bae7gd6+Chr(10)
bae7gd7 = "Dimension Properties: Dimension Precision - Enter the decimal precision for the dimensions value. For example entering 2 will force 1.234 to 1.23. Arrow Size - Set the size of the dimension arrows. Leader Offset - Set the leader offset."
fbcad_desc+=Chr(10)+bae7gd7+Chr(10)
bae8gd1 = "Chamfer: Creates a chamfer, a 50/50 beveled cut out between two lines. The depth of the chamfer is the distance from the initial corner of the two lines to the chamfer's end points. You can use preset chamfer distances by enablining one of the 5 presets or manually enter the chamfer distance each time."
fbcad_desc+=Chr(10)+bae8gd1+Chr(10)
bae8gd2 = "Fillet: Creates a fillet (arc) between selected lines. Similar to the chamfer function, The distance of the fillet is the distance from the initial corner to the end point of the arc. You can use preset distances by enablining one of the 5 presets or manually enter the fillet distance each time."
fbcad_desc+=Chr(10)+bae8gd2+Chr(10)
bae8gd3 = "Best curve fit: Creates a curve best fit. Select two lines first then fillet."
fbcad_desc+=Chr(10)+bae8gd3+Chr(10)
bae8gd4 = "Free hand: chamfer/fillet: With Chamfer or Fillet enabled, two lines are chamfered / fillet at a length determined by tracking the mouse cursor relative to the two line's common point."
fbcad_desc+=Chr(10)+bae8gd4+Chr(10)
bae8gd5 = "Set user chamfer/fillet lengths: Five user chamfer/fillet lengths are available to set."
fbcad_desc+=Chr(10)+bae8gd5+Chr(10)
bae8gd6 = "User set chamfer/fillet length1: With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
fbcad_desc+=Chr(10)+bae8gd6+Chr(10)
bae8gd7 = "User set chamfer/fillet length2: With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
fbcad_desc+=Chr(10)+bae8gd7+Chr(10)
bae8gd8 = "User set chamfer/fillet length3: With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
fbcad_desc+=Chr(10)+bae8gd8+Chr(10)
bae8gd9 = "User set chamfer/fillet length4: With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
fbcad_desc+=Chr(10)+bae8gd9+Chr(10)
bae8gd10 = "User set chamfer/fillet length5: With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
fbcad_desc+=Chr(10)+bae8gd10+Chr(10)
bae9gd1 = "Scale Poperties: Set Scale X & Y Factors to be used when scaling up or down. Default is 2."
fbcad_desc+=Chr(10)+bae9gd1+Chr(10)
bae9gd2 = "Scale UP: Sellected objects increase in scale according to the Scale X & Y Factors."
fbcad_desc+=Chr(10)+bae9gd2+Chr(10)
bae9gd3 = "Scale DOWN: Sellected objects decrease in scale according to the Scale X & Y Factors."
fbcad_desc+=Chr(10)+bae9gd3+Chr(10)
bae9gd4 = "Scale with grab handles: Scale by moving 1 of the 8 grab handles encompasing the selected objects" 
fbcad_desc+=Chr(10)+bae9gd4+Chr(10)
bae10gd1 = "Create Block: Selected entities are saved as a BLOCK. Enter a name for the new block and check Enable Base Point to define the block's base point immediately afterwards: default base point is set to the bottom left corner of the block's bounding box."
fbcad_desc+=Chr(10)+bae10gd1+Chr(10)
bae10gd2 = "Insert Block: Enter the name of a block to insert"
fbcad_desc+=Chr(10)+bae10gd2+Chr(10)
bae10gd3 = "Explode Block: Selected block(s) are changed from a block (or group) to individual entities (or objects)."
fbcad_desc+=Chr(10)+bae10gd3+Chr(10)
bae10gd4 = "Edit block: Edit the selected block"
fbcad_desc+=Chr(10)+bae10gd4+Chr(10)
bae11gd1 = "Grid: Truns the grid visibly on or off"
fbcad_desc+=Chr(10)+bae11gd1+Chr(10)
bae11gd2 = "Snap to Grid: Snap to grid even if it is visibly off"
fbcad_desc+=Chr(10)+bae11gd2+Chr(10)
bae11gd3 = "Grid Properties: Set the grids X & Y Spacing and Offsets"
fbcad_desc+=Chr(10)+bae11gd3+Chr(10)
bae12gd1 = "G-code: Generates g-code from "
fbcad_desc+=Chr(10)+bae12gd1+Chr(10)
bae12gd2 = "G-code Properties: Set the properties for various G-code settings"
fbcad_desc+=Chr(10)+bae12gd2+Chr(10)
bae12gd3 = "Not used yet. Reserved for future gcode functions"
fbcad_desc+=Chr(10)+bae12gd3+Chr(10)
bae12gd4 = "Not used yet. Reserved for future gcode functions"
fbcad_desc+=Chr(10)+bae12gd4+Chr(10)
bae13gd1 = "Move Point: Moves the end point of a line"
fbcad_desc+=Chr(10)+bae13gd1+Chr(10)
bae13gd2 = "Join Points not working yet: Joins or sets the endpoints of two lines to the same point"
fbcad_desc+=Chr(10)+bae13gd2+Chr(10)
bae13gd3 = "Trim: Trims objects - Select objects first, turn trim on and then click object(s) to trim"
fbcad_desc+=Chr(10)+bae13gd3+Chr(10)
bae13gd4 = "Extend - code pending: Extends objects - Select objects first, turn extend on  and then click object(s) to extend"
fbcad_desc+=Chr(10)+bae13gd4+Chr(10)
bae14gd1 = "Parallel single: Creates lines and arcs parallel to objects - which side the paralles are create on is determined by positioning the mouse relative to the initial point of selection. If a Preset parralel offset is not enabled, you will be prompted to enter an offset distance."
fbcad_desc+=Chr(10)+bae14gd1+Chr(10)
bae14gd2 = "Parallel dual: Creates lines and arcs parallel to objects on both sides at the same time. If a Preset parralel offset is not enabled, you will be prompted to enter an offset distance."
fbcad_desc+=Chr(10)+bae14gd2+Chr(10)
bae14gd3 = "Set Parallel offsets: Set up to 3 preset parallel offset distances."
fbcad_desc+=Chr(10)+bae14gd3+Chr(10)
bae14gd4 = "Use offset(1): With this button enabled and either parallel single or dual enabled, parallels are offset at this specified preset distance."
fbcad_desc+=Chr(10)+bae14gd4+Chr(10)
bae14gd5 = "Use offset(2): With this button enabled and either parallel single or dual enabled, parallels are offset at this specified preset distance."
fbcad_desc+=Chr(10)+bae14gd5+Chr(10)
bae14gd6 = "Use offset(3): With this button enabled and either parallel single or dual enabled, parallels are offset at this specified preset distance."
fbcad_desc+=Chr(10)+bae14gd6+Chr(10)
bae14gd7 = "Extend exterior parallel corners: Lines are extended in exterior corners when building parallel paths of a group"
fbcad_desc+=Chr(10)+bae14gd7+Chr(10)
bae14gd8 = "Chamfer parallel corners: Chamfers (bevels) are created in the corners when building parallel paths of a selected group of lines."
fbcad_desc+=Chr(10)+bae14gd8+Chr(10)
bae14gd9 = "Fillet parallel corners: Arcs re created in the corners when building parallel paths of a selected group of lines."
fbcad_desc+=Chr(10)+bae14gd9+Chr(10)
bae15gd1 = "TexT: Insert text. Enter text to insert into the drawing. A basic font with upper case A-Z and 0-9 and a PERIOD are available. Default text size is used if one of the 5 text size presets are not enabled."
fbcad_desc+=Chr(10)+bae15gd1+Chr(10)
bae15gd2 = "Text Size Presets: Enter up to 5 preset sizes for the text." 
fbcad_desc+=Chr(10)+bae15gd2+Chr(10)
bae15gd3 = "Use preset Text Size(1)"
fbcad_desc+=Chr(10)+bae15gd3+Chr(10)
bae15gd4 = "Use preset Text Size(2)"
fbcad_desc+=Chr(10)+bae15gd4+Chr(10)
bae15gd5 = "Use preset Text Size(3)"
fbcad_desc+=Chr(10)+bae15gd5+Chr(10)
bae15gd6 = "Use preset Text Size(4)"
fbcad_desc+=Chr(10)+bae15gd6+Chr(10)
bae15gd7 = "Use preset Text Size(5)"
fbcad_desc+=Chr(10)+bae15gd7+Chr(10)
End Sub
Function mymain() As gboolean
	Dim As Integer i
	If gtkinit=FALSE Then
		gtkinit=TRUE
		baeg_swap(G_OBJECT(bae1gridab),CAST(gpointer, 1))
		baeg_swap(G_OBJECT(bae2gridab),CAST(gpointer, 2))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 3))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 4))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 5))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 6))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 7))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 8))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 9))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 10))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 11))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 12))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 13))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 14))
		baeg_swap(G_OBJECT(bae3gridab),CAST(gpointer, 15))
		'add code for button area - call baeg_swap with new gpointer
		
		layerbuttonclicked=FALSE
		layerstatebuttonclicked=FALSE
		drawingbuttonclicked=FALSE
		snapbuttonclicked=FALSE
		orthbuttonclicked=FALSE
		mcrfbuttonclicked=FALSE
		zpvbuttonclicked=FALSE
		dimbuttonclicked=FALSE
		chamfilbuttonclicked=FALSE
		scalebuttonclicked=FALSE
		blockbuttonclicked=FALSE
		gridbuttonclicked=FALSE
		gcodebuttonclicked=FALSE
		miscbuttonclicked=FALSE
		parallelbuttonclicked=FALSE
		textbuttonclicked=FALSE		
		init_fbcadcam
	EndIf
	
	mousex=mygtkimage_mouse_x
	mousey=mygtkimage_mouse_y
	mousextemp=mousex
	mouseytemp=mousey
	If mousemoved=TRUE Then
		gtk_entry_set_text(GTK_ENTRY(pae1ge1),Str(mousex))
		gtk_entry_set_text(GTK_ENTRY(pae1ge2),Str(mousey))
		mousemoved=FALSE
	EndIf
	'If selbutton=141 Then'animate gcode
	'	ScreenCopy 1,2
	'	ScreenSet 2,2
	'	gcode_showpath
	'	updatefbgfxgtkimage=TRUE
	'	myfbgfxtogtkimg
	'	Sleep 1
	'	Return TRUE
	'Else
	'	main
	'	myfbgfxtogtkimg
	'	Sleep 1
	'	Return TRUE
	'EndIf
	main
	myfbgfxtogtkimg
	Sleep 1
	If buttonson(141)=TRUE Then Sleep 4
	Return TRUE
End Function
sub escapeme()
	Dim i As Integer
	mouse_clicks=0
	If splining=TRUE Then
		If setsplinedown=TRUE Then
			curvesetup()
			setsplinedown=FALSE
		EndIf
		splining=FALSE
		linec=linec-splinec
		'screenset 0,0:view:window
		selbutton=6
		turnbuttonoff
		inview()
		redraw
	End If
	If rayenabled=TRUE And drawing=FALSE Then
		rayenabled=FALSE
		For i = rayi To rayi+7
			lines(i,8)=-1
		Next
		'inview
		'redraw
	End If

	forcelength=FALSE
	drawatangle=FALSE
	forcex=FALSE
	forcey=FALSE
	forcedx=FALSE
	forcedy=FALSE
	if drawmode=true then
		drawing=false
		drawmode=FALSE
		arcstarted=FALSE
		arcing=false
		'screenset 0,0:view:window
		initlinedraw
		redrawoptionboxes=TRUE
	Else
		If modify>0 Then
			modify=-1
			'screenset 0,0:view:window
			For i = 61 To 62
				selbutton=i
				turnbuttonoff
			Next
			If buttonson(109)=TRUE Then
				selbutton=109
				turnbuttonoff
				escapegroupscaling
				groupscaling=FALSE
				groupgrab=FALSE
				inview()
				redraw
				'showgroups
			End If
		Else
			If buttonson(109)=TRUE Then
				'ScreenSet 0,0:view:window
				selbutton=109
				turnbuttonoff
				'escapegroupscaling
				groupscaling=FALSE
				groupgrab=FALSE
				'inview()
				redraw
				'showgroups
			Else
				degroup
				redraw
			End If
		End If
	End If
End Sub
Sub zoomin()
	viewsset=FALSE
	mousexp=mousex-1
	tempmousex=mousexp-1
	If wx2-wx1<maxzoomin Then Exit Sub
	wzoom=(wx2-wx1)/wzoomt
	wx1=wx1+wzoom
	wy1=wy1+wzoom
	wx2=wx2-wzoom
	wy2=wy2-wzoom
	inview()
	redraw
End Sub
Sub zoomout()
	viewsset=FALSE
	mousexp=mousex-1
	tempmousex=mousexp-1
	wzoom=(wx2-wx1)/wzoomt
	wx1=wx1-wzoom
	wy1=wy1-wzoom
	wx2=wx2+wzoom
	wy2=wy2+wzoom
	inview()
	redraw
End Sub
Sub zoominpan()
	viewsset=FALSE
	mousexp=mousex-1
	tempmousex=mousexp-1
	If wx2-wx1<maxzoomin Then Exit Sub
	If selentity=true and snapenable=true Then
		mousex=fxm
		mousey=fym
	EndIf
	panxf=(mousex-wx1)/(wx2-wx1)
	panyf=(mousey-wy1)/(wy2-wy1)
	wx1=wx1+wzoom
	wy1=wy1+wzoom
	wx2=wx2-wzoom
	wy2=wy2-wzoom
	panx=wx1+int((wx2-wx1)*panxf)
	pany=wy1+Int((wy2-wy1)*panyf)
	wx1=wx1+(mousex-panx)
	wx2=wx2+(mousex-panx)
	wy1=wy1+(mousey-pany)
	wy2=wy2+(mousey-pany)
	inview()
	redraw
	wzoom=(wx2-wx1)/wzoomt
	If wzoom<1 Then wzoom=1
End Sub
Sub zoomoutpan()
	viewsset=FALSE
	mousexp=mousex-1
	tempmousex=mousexp-1
	'If wx2-wx1>160000 Then Exit Sub --- what is the limit ???
	If selentity=true and snapenable=true Then
		mousex=fxm
		mousey=fym
	EndIf
	panxf=(mousex-wx1)/(wx2-wx1)
	panyf=(mousey-wy1)/(wy2-wy1)
	wx1=wx1-wzoom
	wy1=wy1-wzoom
	wx2=wx2+wzoom
	wy2=wy2+wzoom
	panx=wx1+Int((wx2-wx1)*panxf)
	pany=wy1+Int((wy2-wy1)*panyf)
	wx1=wx1+(mousex-panx)
	wx2=wx2+(mousex-panx)
	wy1=wy1+(mousey-pany)
	wy2=wy2+(mousey-pany)
	inview()
	redraw
	wzoom=(wx2-wx1)/wzoomt
End Sub
Sub drawlines()
	Dim As Integer i
	For i = 1 To linec
		If i=linec Then
			If drawingline=TRUE Then Exit For
		EndIf
		Line(lines(i,1),lines(i,2))-(lines(i,3),lines(i,4))
	Next
End Sub
Sub drawcircles()
	Dim As Integer i
	For i = 1 To circlec
		If i=circlec Then
			If drawingcircle=TRUE Then Exit For
		EndIf
		circle(circles(i,1),circles(i,2)),circles(i,3)
	Next
End Sub
Sub myfbgfxtogtkimg()'580x580=336400
	If updatefbgfxgtkimage=TRUE Then
		ScreenLock()
		gtk_widget_hide(myimage)
		Dim As Integer i
		Dim As UByte pixcolor,r,g,b
		Dim as ubyte ptr myfbgfx = ScreenPtr
		For i = 0 To mywidth*myheight-1
			pixcolor=*(myfbgfx+i)
			myrgbcol(pixcolor)
			*(mygetpixels+i*3)=myrgb.r
			*(mygetpixels+i*3+1)=myrgb.g
			*(mygetpixels+i*3+2)=myrgb.b
		Next
		ScreenUnlock()
		gtk_widget_show(myimage)
		updatefbgfxgtkimage=FALSE
		'mousemoved=FALSE
	EndIf
End Sub
Sub myrgbcol(pc As UByte)
	Dim As UByte r,g,b
	Select Case pc
		Case 0 'black
			r=0
			g=0
			b=0
		Case 1 'blue
			r=0
			g=0
			b=170
		Case 2 'green
			r=0
			g=170
			b=0
		Case 3 'cyan
			r=0
			g=170
			b=170
		Case 4 'red
			r=170
			g=0
			b=0
		Case 5 'pink
			r=170
			g=0
			b=170
		Case 6 'yellow
			r=170
			g=85
			b=0
		Case 7 'grey
			r=170
			g=170
			b=170
		Case 8 'dark grey
			r=85
			g=85
			b=85
		Case 9 'bright blue
			r=85
			g=85
			b=255
		Case 10 'bright green
			r=85
			g=255
			b=85
		Case 11 'bright cyan
			r=85
			g=255
			b=255
		Case 12 'bright red
			r=255
			g=85
			b=85
		Case 13 'bright pink
			r=255
			g=85
			b=255
		Case 14 'bright yellow
			r=255
			g=255
			b=85
		Case 15 'white
			r=255
			g=255
			b=255
		Case 35
			r=128
			g=0
			b=128
		Case Else
			r=255
			g=255
			b=255
	End Select
	
	myrgb.r=r
	myrgb.g=g
	myrgb.b=b

End Sub
Sub init_fbcadcam()
	Dim As Integer i,x,y
	updatefbgfxgtkimage=TRUE
	drawareax1=0
	drawareax2=579
	drawareay1=0
	drawareay2=579
	wx1=0
	wy1=0
	wx2=drawareax2-drawareax1
	wy2=drawareay2-drawareay1
	wzoomt=10
	wzoom=(wx2-wx1)/wzoomt
	panning=FALSE
	wzoomc=0
	createcircleset=FALSE
	fontpath="fonts\font1\"
	fbcadver="version 2019-02-26 with GTK imptlimentation version 0.84"
	crlf = ""'Chr(13)+Chr(10)
	maxzoomin=10
	maxzoomout=10000
	eliptolerance=.00001'ellips with radius min-maj<eliptolerance are converted to circles or arcs
	jointolerance=.00001
	activelinesc=0
	activecirclesc=0
	activeblocksc=0
	Open "eventlog.txt" For output As #1
	Close #1
	absrel=TRUE
	gplotoffset=TRUE
	gplotoffsetr=1
	gplotoffsetd="outward"
	down=true
	up=FALSE
	drawmode=false
	drawing=false
	ortho=false
	forcex=false
	forcey=false
	forcez=false
	snap=FALSE
	drawingcirclesmethod=1
	showentitytype=TRUE
	instructionaid=FALSE
	ReDim filtertype(3,2)
	filtertype(1,1)="Y"'use filters
	filtertype(1,2)="Use filters"
	filtertype(2,1)="N"'filter all blocks
	filtertype(2,2)="All blocks"'filter all blocks
	filtertype(3,1)="Y"'filter text blocks
	filtertype(3,2)="Text blocks"'filter text blocks
	'filtertyp(4-n)=(y/n) filter by block name
	
	ReDim views(viewsmemstep,4)'used for saved views
	detectsize=.0075
	linememstep=1000
	circlememstep=1000
	layer_active=15
	lc=layer_active
	'turn active layer gtk button on
	g_object_ref(mylayerimageup15)
	gtk_button_set_image(layergb15, mylayerimageup15)
	g_object_ref(mylayerimagedn15)
	gtk_button_set_image(layergb15, mylayerimagedn15)
	scalefactor=2
	scalexfactor=1
	scaleyfactor=1
	scalezfactor=1
	textsize=4
	loadchars=TRUE
	loadbasicblocks=TRUE
	
	dimleaderspacing=10
	dimleadertextoffsetspacing=3
	dimarrowsize=10
	dimprecision=2
	dimensioning=FALSE
	linec=0
	circlec=0
	gridxspacing=10
	gridyspacing=10
	gridxoffset=0
	gridyoffset=0
	grid_spacing_x=gridxspacing
	grid_spacing_y=gridyspacing
	grid_offset_x=gridxoffset
	grid_offset_y=gridyoffset
	showellipsefoci=FALSE
	
	groupgrab=FALSE
	groupscaling=FALSE
	
	redim lines(linememstep,9)
	redim inviewlines(linememstep)
	redim inviewfilterlines(linememstep)
	redim circles(circlememstep,12)
	redim inviewcircles(circlememstep)
	redim inviewfiltercircles(circlememstep)
	blocklevelc=0
	'ReDim blocklevel(blockc,blocklevelc)
	
	'redim blines(linememstep,9)
	'redim bcircles(circlememstep,12)
	
	gravity=9.81
	fbcadupm=1
	trajiv=20
	trajtheta=30
	trajix=0
	trajiy=0
	trajiz=0
	usersetangles(1)=15
	usersetangles(2)=30
	usersetangles(3)=45
	usersetangles(4)=60
	usersetangles(5)=75
	usersetangles(6)=1
	usersetangles(7)=5
	usersetoffsets(1)=10
	usersetoffsets(2)=20
	usersetoffsets(3)=30
	useusersetcflengths(1)=10
	useusersetcflengths(2)=20
	useusersetcflengths(3)=30
	useusersetcflengths(4)=40
	useusersetcflengths(5)=50
	linesingroupc=0
	circlesingroupc=0
	
	layerstate(34)=2'34 is used as the color of rays
	for y=1 to 15
		layerstate(y)=2
	Next
	
	gcode_z_axis_plunge_depth=1
	gcode_Z_axis_steps=1
	gcode_z_feed_rate=100
	gcode_xy_feed_rate=400
	gcode_units=21
	gcode_curve_resolution=3
	
	
	'detect=(wx2-wx1)*.01
	theboxbelow("Launching FBCadCam "+fbcadver+crlf)
	If loadchars=TRUE then
		blockc=0
		For i = 1 To 26
			tempstring=fontpath+Chr(i+64)+".dxf"
			If Len(Dir$(tempstring))<>0 Then
				blockc=blockc+1
				importblockdxf(tempstring)
			End If
		Next
		For i = 0 To 9
			tempstring=fontpath+Chr(i+48)+".dxf"
			If Len(Dir$(tempstring))<>0 Then
				blockc=blockc+1
				importblockdxf(tempstring)
			End If
		Next
		tempstring=fontpath+"PERIOD.dxf"
		If Len(Dir$(tempstring))<>0 Then
			blockc=blockc+1
			importblockdxf(tempstring)
		End If
		fontlinec=linec
		fontcirclec=circlec
		fontblockc=blockc
	End If
	If loadbasicblocks=TRUE Then
		'write me
	EndIf
	blockc=0
	
	
	'8 lines for rays
	rayenabled=FALSE
	rayi=linec+1
	For i = 1 To 8
		memmanageline
		lines(linec,1)=0
		lines(linec,2)=0
		lines(linec,4)=0
		lines(linec,5)=0
		lines(linec,7)=34
		lines(linec,8)=-1
	Next

	'after all preloaded fonts, basic blocks and rays are loaded
	preloadedlinec=linec
	preloadedcirclec=circlec

	selbutton=1
	turnbuttonon
	selbutton=11
	turnbuttonon

	'plottextstring("fbcadcam gtk",0,0,100)
	
	'loadpic

	'load hotkeys
	'Open "user_prefs.txt" For Input As #1
	'Input #1, hotkeyc
	'ReDim hotkeys(hotkeyc)
	'For i = 1 To hotkeyc
	'	Input #1,hotkeys(i)
	'	Line Input #1, tempstring
	'Next
	'Input #1, drawingcirclesmethod
	'drawingcirclesmethod
	''start up options
	''instructional aid
	'Input #1, tempint
	'If tempint=1 Then instructionaid=TRUE Else instructionaid=FALSE
	'Input #1, tempint
	'If tempint=1 Then showentitytype=TRUE Else showentitytype=FALSE

	'Close #1


'hotkeys(1)=120'Asc("x")'Force x manually (initial value is set to mouse-x or snap-x)
'hotkeys(2)=88'Asc("X")'Force x automatically (using mouse-x or snap-x)
'hotkeys(3)=121'Asc("y")'Force y manually (initial value is set to mouse-y or snap-y)
'hotkeys(4)=89'Asc("Y")'Force y automatically (using mouse-y or snap-y)
'hotkeys(5)=122'Asc("z")'Force z manually (initial value is set to mouse-z or snap-z)
'hotkeys(6)=90'Asc("Z")'Force z automatically (using mouse-z or snap-z)
'hotkeys(7)=108'Asc("l")'Force length manually (initial value is set to drawing length)
'hotkeys(8)=76'Asc("L")'Force length manually (initial value is set to sellected object length)
'hotkeys(9)=111'Asc("o")'Ortho mode - draw vertically or horizontally ie.(0,90,180,270 degrees)
'hotkeys(10)=79'Asc("O")'Ortho mode plus angle ie. If angle=10 then (10,100,190,280 degrees)
'hotkeys(11)=97'Asc("a")'Force angle manually (initial value is set to tracked angle or if line is selected then that lines angle) 
'hotkeys(12)=65'Asc("A")'Force angle automatically (value is set to to tracked angle)
'hotkeys(13)=113'Asc("q")'Enable / disable projection of selected object's ortho and ortho plus angle 
'hotkeys(14)=81'Asc("Q")'Enable / disable projection with manually set length of selected object's ortho and ortho plus angle 
'hotkeys(15)=103'Asc("g")'Degroup
'hotkeys(16)=114'Asc("r")'Set rotation angle offset manually (initial value is set to to tracked angle)
'hotkeys(17)=82'Asc("R")'Set rotation angle offset automatically (value is set to to tracked angle)
'hotkeys(18)=105'Asc("i")'Enable / disable instructional aid
'hotkeys(19)=115'Asc("s")'Enable / disable show entity type
'hotkeys(20)=43'Asc("+")'Zoom in
'hotkeys(21)=45'Asc("-")'Zoom out
'hotkeys(22)=327'257'Pan up
'hotkeys(23)=335'258'Pan down
'hotkeys(24)=330'259'Pan left
'hotkeys(25)=332'260'Pan right
'hotkeys(26)=321'256'F8 - view extents
'hotkeys(27)=322''F9 Views - Previous
'hotkeys(27)=510'256'F10 Views - Next
'hotkeys(27)=388''F11 Views - First
'hotkeys(27)=389''F12 Views - Last


	fbcad_desc="FB_CAD_CAM by Owen Reese"+Chr(10)
	fbcad_desc+="fbcadcam.com / opreese@gmail.com"+Chr(10)
	load_button_desc
	gtk_text_buffer_set_text (textinfobuffer, fbcad_desc, -1)
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander1), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander2), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander3), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander4), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander5), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander6), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander7), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander8), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander9), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander10), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander11), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander12), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander13), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander14), false)'not expanded
	'gtk_expander_set_expanded(GTK_EXPANDER(baexpander15), false)'not expanded




	zoomextents
End Sub
Sub theboxbelow(txt As String)
	mytextbuffer=txt+Chr(10)+mytextbuffer
	gtk_text_buffer_set_text (buffer, mytextbuffer, -1)
	ff=FreeFile
	Open "eventlog.txt" For Append As #ff
	Print #ff, txt;
	Close #ff
End Sub
Sub infobox(txt As String)
	If txt<>infoboxtxt Then
		gtk_text_buffer_set_text (textinfobuffer, txt, -1)
		infoboxtxt=txt
	EndIf
End Sub
Sub cpeforlines(line1 As Integer,line2 As Integer)
	'do these two line share a common point
	flx1=lines(line1,1)
	fly1=lines(line1,2)
	flx2=lines(line1,4)
	fly2=lines(line1,5)
	slx1=lines(line2,1)
	sly1=lines(line2,2)
	slx2=lines(line2,4)
	sly2=lines(line2,5)
	cpe=FALSE
	If flx1=slx1 And fly1=sly1 Then
		cpe=TRUE
		cpx=flx1
		cpy=fly1
		cpfl=1
		cpsl=1
	EndIf
	If flx1=slx2 And fly1=sly2 Then
		cpe=TRUE
		cpx=flx1
		cpy=fly1
		cpfl=1
		cpsl=2
	EndIf
	If flx2=slx1 And fly2=sly1 Then
		cpe=TRUE
		cpx=flx2
		cpy=fly2
		cpfl=2
		cpsl=1
	EndIf
	If flx2=slx2 And fly2=sly2 Then
		cpe=TRUE
		cpx=flx2
		cpy=fly2
		cpfl=2
		cpsl=2
	EndIf
	'If cpe=TRUE Then
	'	If cpx=fxm And cpy=fym Then
	'		modify=i
	'		mouse_clicks=1
	'		Exit For
	'	End If
	'Else
	'	cpe=FALSE
	'EndIf
End Sub
Sub buildchamfer()
	newlength=calcd(cpx,cpy,0,mousex,mousey,0)
	If useusersetcflength=TRUE Then newlength=cflength
	'Line(cpx,cpy)-(mousex,mousey)
	Select Case cpfl
		Case 1
			xlength=flx2-cpx
			ylength=fly2-cpy
		Case 2
			xlength=flx1-cpx
			ylength=fly1-cpy
	End Select
	fixflangle
	flfx=cpx+Cos(flangle*d2r)*newlength
	flfy=cpy+sin(flangle*d2r)*newlength
	Select Case cpsl
		Case 1
			xlength=slx2-cpx
			ylength=sly2-cpy
		Case 2
			xlength=slx1-cpx
			ylength=sly1-cpy
	End Select
	fixslangle
	slfx=cpx+Cos(slangle*d2r)*newlength
	slfy=cpy+sin(slangle*d2r)*newlength
End Sub
Sub buildfillet()
	Dim As Integer i
	Dim As BOOLEAN passthrumidarc
	newlength=calcd(cpx,cpy,0,mousex,mousey,0)
	If useusersetcflength=TRUE Then newlength=cflength
	'Line(cpx,cpy)-(mousex,mousey)
	Select Case cpfl
		Case 1
			xlength=flx2-cpx
			ylength=fly2-cpy
		Case 2
			xlength=flx1-cpx
			ylength=fly1-cpy
	End Select
	fixflangle
	flfx=cpx+Cos(flangle*d2r)*newlength
	flfy=cpy+sin(flangle*d2r)*newlength
	Select Case cpsl
		Case 1
			xlength=slx2-cpx
			ylength=sly2-cpy
		Case 2
			xlength=slx1-cpx
			ylength=sly1-cpy
	End Select
	fixslangle
	slfx=cpx+Cos(slangle*d2r)*newlength
	slfy=cpy+sin(slangle*d2r)*newlength
	calcperpintersects(flfx,flfy,flangle,slfx,slfy,slangle)
	'Circle(pfxm,pfym),5
	'draw line between two lines @ newlength
	'Line(flfx,flfy)-(slfx,slfy)
	radius=calcd(flfx,flfy,0,pfxm,pfym,0)
	'take a peek at the circle
	'Circle(pfxm,pfym),radius
	'angle from pfxm,pfym to flfx,flfy
	xlength=flfx-pfxm
	ylength=flfy-pfym
	fixflangle
	'angle from pfxm,pfym to slfx,slfy
	xlength=slfx-pfxm
	ylength=slfy-pfym
	fixslangle
	'angle from pfxm,pfym to cpx,cpy
	xlength=cpx-pfxm
	ylength=cpy-pfym
	fixangle
	'in a counter clock wise directoin
	'make sure the arc starts and goes thru cp(x,y-angle)
	passthrumidarc=FALSE
	For i = Int(flangle) To Int(flangle)+360
		If mymod(i,360)=Int(angle) Then
			'passing thru mid arc
			passthrumidarc=TRUE
		EndIf
		If mymod(i,360)=Int(slangle) Then
			'passing thru slangle
			'if it hasn't passed thru mid arc angle then
			If passthrumidarc=FALSE Then
				Swap flangle,slangle
				Exit For
			EndIf
		EndIf
	Next
	Select Case shift_key
		Case 1,2
			Swap flangle,slangle
	End Select
	arcstart=PI*flangle/180
	arcend=PI*slangle/180
End Sub
Sub trimlinesincf(line1 As Integer,line2 As Integer)
	Select Case mouse_clicks
		Case 1
			lines(line1,8)=-1
			lines(line2,8)=-1
			Select Case cpfl
				Case 1
					lc=lines(line1,7)
					x1=flfx
					y1=flfy
					x2=lines(line1,4)
					y2=lines(line1,5)
					createnewmodlines
					lines(linec,9)=lines(line1,9)
				Case 2
					lc=lines(line1,7)
					x2=flfx
					y2=flfy
					x1=lines(line1,1)
					y1=lines(line1,2)
					createnewmodlines
					lines(linec,9)=lines(line1,9)
			End Select
			Select Case cpsl
				Case 1
					lc=lines(line2,7)
					x1=slfx
					y1=slfy
					x2=lines(line2,4)
					y2=lines(line2,5)
					createnewmodlines
					lines(linec,9)=lines(line2,9)
				Case 2
					lc=lines(line2,7)
					x2=slfx
					y2=slfy
					x1=lines(line2,1)
					y1=lines(line2,2)
					createnewmodlines
					lines(linec,9)=lines(line2,9)
			End Select
		Case 2
			'just in case second click is needed
	End Select
End Sub
Sub setchamferdown(line1 As Integer,line2 As Integer)
	Select Case mouse_clicks
		Case 1
			'set chamfer down
			'compare lc of both lines
			If lines(line1,7)<>lines(line2,7) Then
				lc=14'yellow line to make it stick out
			Else
				lc=lines(line1,7)
			EndIf
			x1=flfx
			y1=flfy
			x2=slfx
			y2=slfy
			createnewmodlines
			'so do i make this new line part of the block???
			'lines(linec,9)=lines(secondlineingroup,9)
			'
			'for now set mouse_clicks to zero
			mouse_clicks=0
			'but if needed set to 2 if second click is needed
		Case 2
			'chamfer is still in dev so:
			'just in case i need a second click
	End Select
End Sub
Sub setfilletdown(line1 As Integer,line2 As Integer)
	Select Case mouse_clicks
		Case 1
			'set fillet down
			'Circle (pfxm,pfym),radius,31,arcstart,arcend
			x1=pfxm
			y1=pfym
			'radius=
			If lines(line1,7)<>lines(line2,7) Then
				lc=14'yellow line to make it stick out
			Else
				lc=lines(line1,7)
			EndIf
			'arcstart=
			'arcend=
			'createcircleset=TRUE'???
			createnewmodcircles(2)
			'for now set mouse_clicks to zero
			mouse_clicks=0
			'but if needed set to 2 if second click is needed
		Case 2
			'fillet is still in dev so:
			'just in case i need a second click
	End Select
End Sub
Sub multichamferfillet(multicf As String)
	Dim As Integer i,j,k
	For k=1 To 2
		For i = preloadedlinec+1 To linec
			If lines(i,8)=1 Then'this line is in the selected group
				'check this line for a common point with any other line in group
				For j=preloadedlinec+1 To linec
					If j<>i Then
						If lines(j,8)=1 Then'this line is also in the selected group
							'do lines i&j share a common point
							cpeforlines(i,j)
							If cpe=TRUE Then'common point exists
								'useusersetcflength=TRUE
								'cflength=20
								mouse_clicks=1
								Select Case multicf
									Case "chamfer"
										buildchamfer
										trimlinesincf(i,j)
										lines(linec-1,8)=1
										lines(linec,8)=1
										setchamferdown(i,j)
									Case "fillet"
										buildfillet
										trimlinesincf(i,j)
										lines(linec-1,8)=1
										lines(linec,8)=1
										setfilletdown(i,j)
								End Select
								Exit For
							EndIf
						EndIf
					EndIf
				Next
			EndIf
		Next
	next
End Sub
Sub main()
 	Dim As Integer i,j,k,x,y
	Dim As Double tempfxm,tempfym
	Dim As BOOLEAN drawingthings,drawingshapes
	lc=layer_active
	actmx=mousex
	actmy=mousey
	'scrnmx=mousex-drawareax1
	'scrnmy=mousey-drawareay1
	scrnmx=mousex
	scrnmy=mousey
	tempmousex=mousex
	tempmousey=mousey
	damx=mousex
	damy=mousey
	drawingthings=FALSE
	drawingshapes=FALSE
	For i = 1 To 6 ' drawing entities
		If buttonson(i)=TRUE Then drawingthings=TRUE
	Next
	For i = 1001 To 1010 'drawing shapes
		If buttonson(i)=TRUE Then
			drawingthings=TRUE
			drawingshapes=TRUE
		EndIf
	Next

	If tempmousex<>mousexp or tempmousey<>mouseyp Or buttonson(141)=TRUE Then
		ScreenCopy 1,2
		ScreenSet 2,2
		If buttonson(141)=TRUE Then gcode_showpath
		updatefbgfxgtkimage=TRUE
		detectingpoints=1
		''showgroups
		detectpoints
		''showgroups
		detectingpoints=0
		If selentity=TRUE Then
			Select Case otd
				Case "line"
					If linesingroupc=0 Then
						For i = 1 To inviewfilterlinesc
							If selline=inviewfilterlines(i) Then
								pae2gsbadj_value_changed(NULL, Cast(gpointer, i))
								Exit For
							EndIf
						Next
					EndIf
				Case "circle","arc","ellipse","elliptical arc"
					If circlesingroupc=0 Then
						For i = 1 To inviewfiltercirclesc
							If selcircle=inviewfiltercircles(i) Then
								pae3gsbadj_value_changed(NULL, Cast(gpointer, i))
								Exit For
							EndIf
						Next
					EndIf
			End Select
		EndIf
		If selentity=FALSE And buttonson(122)=TRUE Then
			'find closest grid point to mouse
			snaptogrid
			fx=gfxm
			fy=gfym
			forcex=TRUE
			forcey=TRUE
		Else
			If selentity=FALSE And forcelength=TRUE Then
				forcex=TRUE
				forcey=TRUE
				xlength=mousex-x1
				ylength=mousey-y1
				fixangle
				newlength=forcedlength
				atolength
			EndIf
		End If
	End If
	'the blue lines
	If forcedx=TRUE Then
		fxm=fxd
		fx=fxd
	EndIf
	If forcedy=TRUE Then
		fym=fyd
		fy=fyd
	EndIf
	If forcex=TRUE And selentity=FALSE Then
		gtk_entry_set_text(GTK_ENTRY(pae1ge3),Str(fx))
	Else
		If selentity=TRUE Then
			gtk_entry_set_text(GTK_ENTRY(pae1ge3),Str(fxm))
		Else
			gtk_entry_set_text(GTK_ENTRY(pae1ge3),"")
		EndIf
	EndIf
	
	If forcey=TRUE And selentity=FALSE Then
		gtk_entry_set_text(GTK_ENTRY(pae1ge4),Str(fy))
	Else
		If selentity=TRUE Then
			gtk_entry_set_text(GTK_ENTRY(pae1ge4),Str(fym))
		Else
			gtk_entry_set_text(GTK_ENTRY(pae1ge4),"")
		EndIf
	EndIf
	
	Select Case mouseb
		case 0'no mouse buttons are down
			If createcircleset=TRUE Then
				createcircleset=FALSE
				escapeme
			End If
			If mousew<>mousewp Then
				'maybe do some timing for zooming
				'in order to limit redraws and inviews
				If mousewp<mousew Then
					zoominpan
				Else
					zoomoutpan
				End If
				mousewp=mousew
			EndIf
			if tempmousex<>mousexp or tempmousey<>mouseyp Then
				If panning=TRUE Then
					panning=FALSE
					redraw
				End If
				If spliningmovingpoints=TRUE Then
					spliningmovingpoints=FALSE
					selbutton=6
					turnbuttonon()
					x1=splinemovingpointsx1
					y1=splinemovingpointsy1
				EndIf
				If buttonson(21)=TRUE Then ortho=TRUE
				If buttonson(22)=TRUE Then drawatangle=TRUE
				'mouse button was pressed down (user may or may not have released the mouse button yet reguardless mouseb has been set to zero)
				if drawingthings=TRUE Then'in the process of drawing entities or shapes
					if drawmode=TRUE Then
						if mousex<>mousexp or mousey<>mouseyp then
							'ScreenCopy 1,2
							'ScreenSet 2,2
							'updatefbgfxgtkimage=TRUE
							drawing=TRUE
							x2=mousex
							y2=mousey
							if ortho=true Then
								'trackangle
								xlength=x2-x1
								ylength=y2-y1
								fixangle
								'If buttonson(22)=FALSE Then
								'	Select Case Int(angle)
								'		Case 315 To 360,0 To 45
								'			angle=0
								'			length=x2-x1
								'			y2=y1
								'		Case 46 To 135
								'			angle=90
								'			length=y2-y1
								'			x2=x1
								'		Case 136 To 225
								'			angle=180
								'			length=x1-x2
								'			y2=y1
								'		Case 226 To 314
								'			angle=270
								'			length=y1-y2
								'			x2=x1
								'	End Select
								'	fx=x2
								'	fy=y2
								'Else
									If Abs(angle-angle2)>180 Then
										If angle<angle2 Then
											angle=angle+360
										Else
											angle2=angle2+360
										EndIf
									EndIf
									If angle>angle2+45 Then
										angle2=angle2+90
										If angle2>360 Then angle2=angle2-360
									Else
										If angle<angle2-45 Then
											angle2=angle2-90
											If angle2<0 then angle2=360+angle2
										EndIf
									EndIf
									drawatangle=TRUE
								'EndIf
							EndIf
							If buttonson(53)=TRUE Then
								Select Case fromotd
									Case "line"
										angle2=perpangle+90
										altox2y2
										pfxm=fx
										pfym=fy
										angle2=perpangle-90
										altox2y2
										If calcd(x2,y2,0,pfxm,pfym,0)<calcd(x2,y2,0,fx,fy,0) Then angle2=perpangle+90
									Case "circle","arc","ellipse","elliptical arc"
										angle2=perpangle+180
										altox2y2
										pfxm=fx
										pfym=fy
										angle2=perpangle
										altox2y2
										If calcd(x2,y2,0,pfxm,pfym,0)<calcd(x2,y2,0,fx,fy,0) Then angle2=perpangle+180
									'Case "arc"
									'Case "ellipse","elliptical arc"
										
								End Select
							EndIf
							If buttonson(54)=TRUE Then
								Select Case fromotd
									Case "circle","arc","ellipse","elliptical arc"
										angle2=perpangle+90
										altox2y2
										pfxm=fx
										pfym=fy
										angle2=perpangle+270
										altox2y2
										If calcd(x2,y2,0,pfxm,pfym,0)<calcd(x2,y2,0,fx,fy,0) Then angle2=perpangle+90
								End Select
							EndIf
							If drawatangle=TRUE Then
								forcex=TRUE
								forcey=TRUE
								If forcelength=TRUE Then
									newlength=forcedlength
									atolength2
								Else
									If forcedx=TRUE Then
										altofxm
									Else
										If forcedy=TRUE Then
											altofym
										Else
											altox2y2
										EndIf
									EndIf
									
								EndIf
							EndIf
							If forcex=FALSE Then forcedx=FALSE
							If forcey=FALSE Then forcedy=FALSE
							if forcex=true then x2=fx
							if forcey=true then y2=fy
							if selentity=true and snapenable=true then
								x2=fxm
								y2=fym
								fx=fxm
								fy=fym
							end If
							If selentity=TRUE And buttonson(122)=TRUE Then
								fx=fxm
								fy=fym
							EndIf
							If drawatangle=TRUE Then
								infobox("(X1,Y1) "+Str(x1)+", "+Str(y1)+Chr(10)+"(X2,Y2) "+Str(x2)+", "+Str(y2)+Chr(10)+"Angle "+Str(angle2)+Chr(10)+"Length "+Str(length))
							Else
								infobox("(X1,Y1) "+Str(x1)+", "+Str(y1)+Chr(10)+"(X2,Y2) "+Str(x2)+", "+Str(y2)+Chr(10)+"Angle "+Str(angle)+Chr(10)+"Length "+Str(length))
							EndIf
							if buttonson(2)=true Or buttonson(4)=true Or buttonson(5)=true Then
								Line (x1,y1)-(x2,y2),42
							EndIf
							If buttonson(3)=TRUE Then
								If mouse_clicks<3 Then Line (x1,y1)-(x2,y2),42
							EndIf
							If buttonson(1)=true Or buttonson(6)=TRUE Then
								Line (x1,y1)-(x2,y2),lc
							EndIf
							If buttonson(2)=true Then
								Select Case drawingcirclesmethod
									Case 1
										Select Case shift_key
											Case 0
												If selentity=TRUE Then
													length = sqr((fxm-x1)^2 + (fym-y1)^2 + (fzm-z1)^2)
												Else
													length = sqr((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
												EndIf
												Circle (x1,y1),length,lc
											Case 1,2
												Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
										End select
									Case 2
										Select Case shift_key
											Case 1,2
												If selentity=TRUE Then
													length = sqr((fxm-x1)^2 + (fym-y1)^2 + (fzm-z1)^2)
												Else
													length = sqr((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)
												EndIf
												Circle (x1,y1),length,lc
											Case 0
												Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
										End select
								End Select
							EndIf
							'mouse button is up
							If buttonson(3)=TRUE Then
								Select case mouse_clicks
									Case 0
										If arcstarted=TRUE Then
											mouse_clicks=1
											If buttonson(53)=TRUE And selentity=TRUE Then
												fromotd=otd
												forcex=TRUE
												forcey=TRUE
												drawatangle=TRUE
												Select Case otd
													Case "line"
														perpangle=selangle
												End Select
											EndIf
										End If
									Case 1
										'defining the arc radius and center
										'Circle (x1,y1),length,31
										Select Case drawingcirclesmethod
											Case 1
												Select Case shift_key
													Case 0
														If selentity=TRUE Then
															length = sqr((fxm-x1)^2 + (fym-y1)^2 + (fzm-z1)^2)
														EndIf
														Circle (x1,y1),length,lc
													Case 1,2
														Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
												End select
											Case 2
												Select Case shift_key
													Case 1,2
														If selentity=TRUE Then
															length = sqr((fxm-x1)^2 + (fym-y1)^2 + (fzm-z1)^2)
														EndIf
														Circle (x1,y1),length,lc
													Case 0
														Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
												End select
										End Select
										If arcsetradius=TRUE Then mouse_clicks=2
									Case 2
										Circle (x1,y1),radius,lc
										Line(x1,y1)-(x2,y2)
										If arcing=TRUE Then mouse_clicks=3
									Case 3
										arcstarttemp=arcstart
										'drawing the arc
										'if drawing the arc to another object then
										If selentity=TRUE Then
											xlength=fxm-x1
											ylength=fym-y1
											fixangle
											If buttonson(20)=TRUE Then
												'button 20 is new. snap to where the arc would intersect object
												'caclulate the intersection of the arc (or circle) with
												'the sellected object (line)
												'calculate that points angle relative to the center of
												'the circle
												'set arcend to that angle
												memmanagecircle
												circles(circlec,1)=x1
												circles(circlec,2)=y1
												circles(circlec,4)=radius
												'circles(circlec,5)=lc
												circles(circlec,9)=1
												bintersect=circlec
												Select Case otd
													Case "line"
														'aintersect is the line
														'bintersect is the circle
														aintersect=selline
														'bintersect=circlec
														calclinecircleintersection
													Case "circle"
														aintersect=selcircle
														calccirclecircleintersection
													Case "ellipse"
														aintersect=selcircle
														calccircleellipseintersection()
												End Select
												circlec=circlec-1
	'															'get the angle of mouse to circle
												xlength=fxm-x1
												ylength=fym-y1
												fixangle
												Line (fxm-detect,fym-detect)-(fxm+detect,fym+detect),14,b
											End If
											Line(x1,y1)-(fxm,fym),42
										Else
											Line(x1,y1)-(x2,y2),42
										EndIf
										If buttonson(53)=TRUE Then
											If angle2<0 Then angle2=360+angle2
											arcend=PI*angle2/180
										Else
											arcend=PI*angle/180
										EndIf
										arcstart=arcstarttemp
										Circle (x1,y1),radius,lc,arcstart,arcend
										if buttonson(76)=TRUE Then
	'														dimensionxy(1,1)=modifyx1
	'														dimensionxy(1,2)=modifyy1
	'													Case 2
	'														dimensionxy(2,1)=modifyx2
	'														dimensionxy(2,2)=modifyy2
	'														dimensioningclicks=3
											dimensionxy(1,1)=x1+cos(arcstart)*radius
											dimensionxy(1,2)=y1+sin(arcstart)*radius
											dimensionxy(2,1)=x1+cos(arcend)*radius
											dimensionxy(2,2)=y1+sin(arcend)*radius
											'Circle (dimensionxy(1,1) ,dimensionxy(1,2) ),10,10
											'Circle (dimensionxy(2,1) ,dimensionxy(2,2) ),10,10
											'(arcstart*180/pi+90)
											'(arcend*180/pi+90)
											If arcstart > arcend Then
												'dimvalue=mymod((arcstart*180/pi+90)+(arcend*180/pi+90),360)
												dimvalue=mymod(360-(arcstart*180/pi)+(arcend*180/pi),360)
											Else
												'dimvalue=mymod((arcstart*180/pi+90)-(arcend*180/pi+90),360)
												dimvalue=mymod((arcend*180/pi)-(arcstart*180/pi),360)
											EndIf
											'dimvalue=Abs(dimvalue)
											adjustprecision
											tempstring=Str(dimvalue)
											Draw String (mousex,mousey),tempstring
											'acrstart arrow
											fx=dimensionxy(1,1)+cos((arcstart*180/pi+90)*d2r)*dimarrowsize
											fy=dimensionxy(1,2)+sin((arcstart*180/pi+90)*d2r)*dimarrowsize
											dimensionxy(3,1)=fx+cos((arcstart*180/pi+180)*d2r)*dimarrowsize/2
											dimensionxy(3,2)=fy+sin((arcstart*180/pi+180)*d2r)*dimarrowsize/2
											dimensionxy(4,1)=fx+cos((arcstart*180/pi)*d2r)*dimarrowsize/2
											dimensionxy(4,2)=fy+sin((arcstart*180/pi)*d2r)*dimarrowsize/2
											Line (dimensionxy(3,1) ,dimensionxy(3,2) )-(dimensionxy(4,1) ,dimensionxy(4,2) )
											Line (dimensionxy(3,1) ,dimensionxy(3,2) )-(dimensionxy(1,1) ,dimensionxy(1,2) )
											Line (dimensionxy(4,1) ,dimensionxy(4,2) )-(dimensionxy(1,1) ,dimensionxy(1,2) )
											'arcend arrow
											fx=dimensionxy(2,1)+cos((arcend*180/pi-90)*d2r)*dimarrowsize
											fy=dimensionxy(2,2)+sin((arcend*180/pi-90)*d2r)*dimarrowsize
											dimensionxy(5,1)=fx+cos((arcend*180/pi-180)*d2r)*dimarrowsize/2
											dimensionxy(5,2)=fy+sin((arcend*180/pi-180)*d2r)*dimarrowsize/2
											dimensionxy(6,1)=fx+cos((arcend*180/pi)*d2r)*dimarrowsize/2
											dimensionxy(6,2)=fy+sin((arcend*180/pi)*d2r)*dimarrowsize/2
											Line (dimensionxy(5,1) ,dimensionxy(5,2) )-(dimensionxy(6,1) ,dimensionxy(6,2) )
											Line (dimensionxy(5,1) ,dimensionxy(5,2) )-(dimensionxy(2,1) ,dimensionxy(2,2) )
											Line (dimensionxy(6,1) ,dimensionxy(6,2) )-(dimensionxy(2,1) ,dimensionxy(2,2) )
										End If
								End Select
							End If
							'no mouse button down
							If buttonson(4)=true Then
								If ellipsstarted=false Then
									ellipsing=true
									'Circle (x1,y1),length,31
									Select Case drawingcirclesmethod
										Case 1
											Select Case shift_key
												Case 0
													Circle (x1,y1),length,lc
												Case 1,2
													Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
											End Select
										Case 2
											Select Case shift_key
												Case 1,2
													Circle (x1,y1),length,lc
												Case 0
													Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
											End Select
									End Select
									'If buttonson(53)=TRUE Then
									'	If angle2<0 Then angle2=360+angle2
									'	arcend=PI*angle2/180
									'Else
									'	arcend=PI*angle/180
									'EndIf
								Else
									Line(x1,y1)-(x2,y2)
									eradius=Sqr((x1-x2)^2 + (y1-y2)^2)
									ellipsing=false
									plotellipse(x1,y1,z1,initradius,lc,initarcstart,initarcend,eradius,initerotation)
								EndIf
							EndIf
							'no mouse button down
							If buttonson(5)=true Then
								If ellipsstarted=false Then
									ellipsing=true
									'Circle (x1,y1),length,31
									Select Case drawingcirclesmethod
										Case 1
											Select Case shift_key
												Case 0
													Circle (x1,y1),length,lc
												Case 1,2
													Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
											End Select
										Case 2
											Select Case shift_key
												Case 1,2
													Circle (x1,y1),length,lc
												Case 0
													Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,lc
											End Select
									End Select
								Else
									Line(x1,y1)-(x2,y2)
									ellipsing=false
									If arcstarted=false Then
										If ellipsarcing=TRUE Then
											arcing=TRUE
										else'no mouse buttons are down
											eradius=Sqr((x1-x2)^2 + (y1-y2)^2)
										EndIf
										plotellipse(x1,y1,z1,initradius,lc,0,360,eradius,initerotation)
									Else'no mouse buttons are down
										arcing=FALSE
										'Line (x1,y1)-(x2,y2)
										Select Case shift_key
											Case 0
												'pickup
												'this is an idea from way back when
												'user has the option to have ellipse arc start / end
												'follow the angle derived from ellipse center
												'to mouse x,y
												'or not
												'note: using drawingcirclesmethod is not used here
	'																Locate 50,50
	'																Print x2,y2
												x1p=x2-x1
												y1p=(y2-y1)
												x2=x1p*Cos(-initerotation*d2r) - y1p*Sin(-initerotation*d2r)+x1
												y2=y1p*Cos(-initerotation*d2r) + x1p*Sin(-initerotation*d2r)+y1
												'Locate 51,50
												'Print x2,y2
	'																If radius<eradius then
	'																	ylength=(y2-y1)*(radius/eradius)
	'																	xlength=x2-x1
	'																Else'no mouse buttons are down
	'																	ylength=(y2-y1)
	'																	xlength=(x2-x1)*(eradius/radius)
	'																End if
												ylength=(y2-y1)*(initradius/initeradius)
												xlength=x2-x1
												fixangle
												arcend=angle
												'Locate 60,60
												'Print arcend
											Case 1,2
												arcend=angle-initerotation
										End Select
										'Locate 50,50
										'Print arcend
										plotellipse(x1,y1,z1,initradius,lc,initarcstart,arcend,initeradius,initerotation)
									EndIf
								EndIf
							EndIf
							If buttonson(1001)=TRUE Then'Equilateral Triangle
								show_polygon(3)
							EndIf
							'no mouse buttons are down
							If buttonson(1002)=TRUE Then'Right Triangle
								draw_right_triangle
							EndIf
							If buttonson(1003)=TRUE Then'Square
								show_polygon(4)
							EndIf
							If buttonson(1004)=TRUE Then'Rectangle
								draw_rectangle
							EndIf
							If buttonson(1005)=TRUE Then'Rhombus
								draw_rhombus
							EndIf
							If buttonson(1006)=TRUE Then'Parallelogram
								draw_parallelogram
							EndIf
							If buttonson(1007)=TRUE Then'
								show_polygon(5)
							EndIf
							If buttonson(1008)=TRUE Then'
								show_polygon(6)
							EndIf
							If buttonson(1009)=TRUE Then'
								show_polygon(7)
							EndIf
							If buttonson(1010)=TRUE Then'Octogon
								show_polygon(8)
							EndIf
						end If
					Else
						if buttonson(122)=TRUE Then
							snapenable=true
							if selentity=FALSE Then
								selentity=true
								fxm=fx
								fym=fy
							EndIf
						EndIf
					End If
					'no mouse button down
				else'not drawing lines - detecting points and modifiying group
					If boxselect=true Then
						boxing=TRUE
						'draw rectangel
						Line(boxselectx1,boxselecty1)-(mousex,mousey),31,b
					Else
						boxing=false
					End If
					if selentity=true and snapenable=true Then
						If movingpts=FALSE Then movepoints
						modifyx2=fxm
						modifyy2=fym
						modifyz2=fzm
					Else
						modifyx2=mousex
						modifyy2=mousey
						modifyz2=mousez
						if ortho=TRUE And modify>0 Then
							x2=mousex
							y2=mousey
							xlength=x2-x1
							ylength=y2-y1
							fixangle 'this maybe a proplem as angle was negative sometimes
							If Abs(angle-angle2)>180 Then
								If angle<angle2 Then
									angle=angle+360
								Else
									angle2=angle2+360
								EndIf
							EndIf
							If angle>angle2+45 Then
								angle2=angle2+90
								If angle2>360 Then angle2=angle2-360
							Else
								If angle<angle2-45 Then
									angle2=angle2-90
									If angle2<0 then angle2=360+angle2
								EndIf
							EndIf
							drawatangle=TRUE
							forcex=TRUE
							forcey=TRUE
						end If
						if drawatangle=true And modify>0 then
							x1=modifyx1
							x2=modifyx2
							y1=modifyy1
							y2=modifyy2
							altox2y2
							forcex=TRUE'this was added 1-22-11
							forcey=TRUE'this was added 1-22-11
						end if
						if forcex=true then modifyx2=fx
						if forcey=true then modifyy2=fy
						If modify>0 Then'this was added 1-22-11
							x2=modifyx2
							y2=modifyy2
						EndIf
					end If
					'mouse button=0
					select case modify
						case -1
							modify=0
						case 31,32'move group'**
							Line(modifyx1,modifyy1)-(modifyx2,modifyy2)
							modifying=true
							movegroup
						case 33,34'rotating group
							Line(modifyx1,modifyy1)-(modifyx2,modifyy2)
							modifying=TRUE
							If drawatangle=TRUE Then angle=angle2
							rotategroup
						case 35
							flipvertical
							'screenset 0,0:view:window
							selbutton=35
							turnbuttonoff
							modifying=false
							modify=-1
							redraw
						case 36
							fliphorizontal
							'screenset 0,0:view:window
							selbutton=36
							turnbuttonoff
							modifying=false
							modify=-1
							redraw
						case 37
							flipverticalcopy
							'screenset 0,0:view:window
							selbutton=37
							turnbuttonoff
							modifying=false
							modify=-1
							redraw
						case 38
							fliphorizontalcopy
							'screenset 0,0:view:window
							selbutton=38
							turnbuttonoff
							modifying=false
							modify=-1
							redraw
						case 39
							flipvh
							'screenset 0,0:view:window
							selbutton=39
							turnbuttonoff
							modifying=false
							modify=-1
							redraw
						case 40
							flipvhcopy
							'screenset 0,0:view:window
							selbutton=40
							turnbuttonoff
							modifying=false
							modify=-1
							redraw
						Case 51
							modifying=true
							movepointstomouse
						Case 55
							modifying=true
							Select Case fromotd
								Case "line"
									angle2=perpangle+90
									aftom
									pfxm=fx
									pfym=fy
									angle2=perpangle-90
									aftom
									If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then angle2=perpangle+90
									Line(lines(paraselline,1)+cos(angle2*d2r)*para_offset, lines(paraselline,2)+sin(angle2*d2r)*para_offset)-(lines(paraselline,4)+cos(angle2*d2r)*para_offset, lines(paraselline,5)+sin(angle2*d2r)*para_offset)
								Case "circle","arc","ellipse","elliptical arc"
									'angle2=perpangle+180
									'aftom
									'pfxm=fx
									'pfym=fy
									'angle2=perpangle
									'aftom
									'If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then angle2=perpangle+180
									'para_radius=circles(paraselcircle,4)+length
									'circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5),circles(paraselcircle,6),circles(paraselcircle,7)
									para_radius=circles(paraselcircle,4)
									Select Case calcd(mousex,mousey,0,circles(paraselcircle,1),circles(paraselcircle,2),0)
										Case Is < para_radius
											para_radius=para_radius-para_offset
										Case Else
											para_radius=para_radius+para_offset
									End Select
									Select Case circles(paraselcircle,9)
										Case 1
											circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5)
										Case 2
											circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5),circles(paraselcircle,6),circles(paraselcircle,7)
										Case 3,4
											theboxbelow("plot para ellipse")
											'plotoffsetellipse(
											plotellipseoffset(circles(paraselcircle,1),circles(paraselcircle,2),circles(paraselcircle,3),para_radius,Int(circles(paraselcircle,5)),circles(paraselcircle,6),circles(paraselcircle,7),circles(paraselcircle,8),circles(paraselcircle,11))
									End Select
							End Select
						Case 61'not drawing lines - detecting points and modifiying group
							'no mouse buttons are down
							'modifying=TRUE
							trimobjects
						Case 62
							'modifying=TRUE
							extendobjects
						Case 71 To 80'dimension distance between 2 points
							modifying=TRUE
							select case modify
								Case 71 To 75
									Select Case dimensioningclicks
										Case 1
											dimensionxy(1,1)=modifyx1
											dimensionxy(1,2)=modifyy1
										Case 2
											dimensionxy(2,1)=modifyx2
											dimensionxy(2,2)=modifyy2
											dimensioningclicks=3
									End Select
							End Select
							select case modify
								Case 71 'dimension distance
									If dimensioningclicks<>1 Then
										'calcperp from mousex,y to line(dimx1,y1)-(dimx2,y2)
										memmanageline
										lines(linec,1)=dimensionxy(1,1)
										lines(linec,2)=dimensionxy(1,2)
										lines(linec,4)=dimensionxy(2,1)
										lines(linec,5)=dimensionxy(2,2)
										selline=linec
										calcselangle
										x1=mousex
										y1=mousey
										calcperpendicular
										lines(linec,1)=fxm
										lines(linec,2)=fym
										lines(linec,4)=mousex
										lines(linec,5)=mousey
										selline=linec
										calcselangle
										calcsellength
										x1=dimensionxy(1,1)
										y1=dimensionxy(1,2)
										altosellength'returns fx,fy
										dimensionxy(4,1)=fx
										dimensionxy(4,2)=fy
										'Line(x1,y1)-(fx,fy)
										x1=dimensionxy(2,1)
										y1=dimensionxy(2,2)
										altosellength'returns fx,fy
										dimensionxy(5,1)=fx
										dimensionxy(5,2)=fy
										'Line(x1,y1)-(fx,fy)
										linec=linec-1
										Line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(4,1),dimensionxy(4,2))
										Line(dimensionxy(2,1),dimensionxy(2,2))-(dimensionxy(5,1),dimensionxy(5,2))
										Line(dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(5,1),dimensionxy(5,2))
										dimvalue=calcd(dimensionxy(1,1),dimensionxy(1,2),dimensionxy(1,3),dimensionxy(2,1),dimensionxy(2,2),dimensionxy(2,3))
										'tempstring=Str(tempdouble)
										adjustprecision
										'Draw String (mousex,mousey),tempstring
									Else
										Line(dimensionxy(1,1),dimensionxy(1,2))-(mousex,mousey)
									End If
								Case 72 'dimension X axis
									If dimensioningclicks=1 Then
										Line(dimensionxy(1,1),dimensionxy(1,2))-(mousex,dimensionxy(1,2))
									Else
										Line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(1,1),mousey)
										Line(dimensionxy(2,1),dimensionxy(2,2))-(dimensionxy(2,1),mousey)
										Line(dimensionxy(1,1),mousey)-(dimensionxy(2,1),mousey)
										dimvalue=dimensionxy(1,1)-dimensionxy(2,1)'calcd(dimensionxy(1,1),dimensionxy(1,2),dimensionxy(1,3),dimensionxy(2,1),dimensionxy(2,2),dimensionxy(2,3))
										'tempstring=Str(tempdouble)
										adjustprecision
										'Draw String (mousex,mousey),tempstring
									End If
								Case 73 'dimension Y axis
									If dimensioningclicks=1 Then
										Line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(1,1),mousey)
									Else
										Line(dimensionxy(1,1),dimensionxy(1,2))-(mousex,dimensionxy(1,2))
										Line(dimensionxy(2,1),dimensionxy(2,2))-(mousex,dimensionxy(2,2))
										Line(mousex,dimensionxy(1,2))-(mousex,dimensionxy(2,2))
										dimvalue=dimensionxy(1,2)-dimensionxy(2,2)'calcd(dimensionxy(1,1),dimensionxy(1,2),dimensionxy(1,3),dimensionxy(2,1),dimensionxy(2,2),dimensionxy(2,3))
										'tempstring=Str(tempdouble)
										adjustprecision
										'Draw String (mousex,mousey),tempstring
									End If
								Case 74'dimension radius
									If dimensioningclicks=1 Then
										If shift_key=1 Or shift_key=2 Then
											'draw cross hairs at center of circle
											Line(dimensionxy(1,1)-10,dimensionxy(1,2))-(dimensionxy(1,1)+10,dimensionxy(1,2))
											Line(dimensionxy(1,1),dimensionxy(1,2)-10)-(dimensionxy(1,1),dimensionxy(1,2)+10)
											line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(3,1),dimensionxy(3,2))
											line(dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(5,1),dimensionxy(5,2))
											line(dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(2,1),dimensionxy(2,2))
											line(dimensionxy(5,1),dimensionxy(5,2))-(dimensionxy(2,1),dimensionxy(2,2))
											dimensionxy(6,1)=mousex
											dimensionxy(6,2)=mousey
											line(dimensionxy(2,1),dimensionxy(2,2))-(dimensionxy(6,1),dimensionxy(6,2))
											dimvalue=circles(tempselcircle,4)
											'tempstring=Str(tempdouble)
											adjustprecision
											'Draw String (mousex,mousey),tempstring
										Else
											'draw cross hairs at center of circle
											Line(dimensionxy(1,1)-10,dimensionxy(1,2))-(dimensionxy(1,1)+10,dimensionxy(1,2))
											Line(dimensionxy(1,1),dimensionxy(1,2)-10)-(dimensionxy(1,1),dimensionxy(1,2)+10)
											'line(dimensionxy(1,1),dimensionxy(1,2))-(mousex,mousey)
											memmanageline
											
											lines(linec,1)=dimensionxy(1,1)
											lines(linec,2)=dimensionxy(1,2)
											lines(linec,4)=mousex
											lines(linec,5)=mousey
											selline=linec
											linec=linec-1
											calcselangle
											'calcsellength
											sellength=circles(tempselcircle,4)
											x1=dimensionxy(1,1)
											y1=dimensionxy(1,2)
											altosellength'returns fx,fy
											dimensionxy(2,1)=fx
											dimensionxy(2,2)=fy
											'line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(2,1),dimensionxy(2,2))
											
											'now make the arrow
											'dimleaderspacing,dimleadertextoffsetspacing,dimarrowsize
											sellength=circles(tempselcircle,4)-dimarrowsize
											x1=dimensionxy(1,1)
											y1=dimensionxy(1,2)
											altosellength'returns fx,fy
											dimensionxy(3,1)=fx
											dimensionxy(3,2)=fy
											line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(3,1),dimensionxy(3,2))
											sellength=dimarrowsize/2
											x1=dimensionxy(3,1)
											y1=dimensionxy(3,2)
											selangle=selangle+90
											altosellength'returns fx,fy
											dimensionxy(4,1)=fx
											dimensionxy(4,2)=fy
											x1=dimensionxy(3,1)
											y1=dimensionxy(3,2)
											selangle=selangle+180
											altosellength'returns fx,fy
											dimensionxy(5,1)=fx
											dimensionxy(5,2)=fy
											line(dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(5,1),dimensionxy(5,2))
											line(dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(2,1),dimensionxy(2,2))
											line(dimensionxy(5,1),dimensionxy(5,2))-(dimensionxy(2,1),dimensionxy(2,2))
											dimensionxy(6,1)=mousex
											dimensionxy(6,2)=mousey
											line(dimensionxy(2,1),dimensionxy(2,2))-(dimensionxy(6,1),dimensionxy(6,2))
											dimvalue=circles(tempselcircle,4)
											'tempstring=Str(tempdouble)
											adjustprecision
											'Draw String (mousex,mousey),tempstring
										EndIf
									End If
								Case 75
									'dimension diameter - no mouse button down
									If dimensioningclicks=1 Then
										'no need for cross hairs for diameter
										'draw cross hairs at center of circle
	'																Line(dimensionxy(1,1)-10,dimensionxy(1,2))-(dimensionxy(1,1)+10,dimensionxy(1,2))
	'																Line(dimensionxy(1,1),dimensionxy(1,2)-10)-(dimensionxy(1,1),dimensionxy(1,2)+10)
	'																'line(dimensionxy(1,1),dimensionxy(1,2))-(mousex,mousey)
										memmanageline
										
										lines(linec,1)=dimensionxy(1,1)
										lines(linec,2)=dimensionxy(1,2)
										lines(linec,4)=mousex
										lines(linec,5)=mousey
										selline=linec
										linec=linec-1
										calcselangle
										'calcsellength
										sellength=circles(tempselcircle,4)
										x1=dimensionxy(1,1)
										y1=dimensionxy(1,2)
										altosellength'returns fx,fy
										'this is the second end of the diameter dimension line
										dimensionxy(5,1)=fx
										dimensionxy(5,2)=fy
										'line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(2,1),dimensionxy(2,2))
										selangle=selangle+180
										'calcsellength
										sellength=circles(tempselcircle,4)
										x1=dimensionxy(1,1)
										y1=dimensionxy(1,2)
										altosellength'returns fx,fy
										'this is the first end of the diameter dimension line
										dimensionxy(4,1)=fx
										dimensionxy(4,2)=fy
										'Circle (dimensionxy(5,1),dimensionxy(5,2)),10,10
										'copy array 1 & 2 to 4 & 5
										'below is a copy of case 71 set dim down
										'dim array 4 & 5 is used to calc arrow positions
										Dim As Double tempdx1,tempdy1,tempdx2,tempdy2
										tempdx1=dimensionxy(4,1)
										tempdy1=dimensionxy(4,2)
										tempdx2=dimensionxy(5,1)
										tempdy2=dimensionxy(5,2)
										'line between legs
										memmanageline
										'making room for the arrow
										'shorten up the left side
										lines(linec,1)=dimensionxy(4,1)
										lines(linec,2)=dimensionxy(4,2)
										lines(linec,4)=dimensionxy(5,1)
										lines(linec,5)=dimensionxy(5,2)
										selline=linec
										calcselangle
										calcsellength
										sellength=sellength-dimarrowsize
										x1=dimensionxy(4,1)
										y1=dimensionxy(4,2)
										altosellength'returns fx,fy
										dimensionxy(5,1)=fx
										dimensionxy(5,2)=fy
										'shorten up the right side
										lines(linec,1)=dimensionxy(5,1)
										lines(linec,2)=dimensionxy(5,2)
										lines(linec,4)=dimensionxy(4,1)
										lines(linec,5)=dimensionxy(4,2)
										selline=linec
										calcselangle
										calcsellength
										sellength=sellength-dimarrowsize
										x1=dimensionxy(5,1)
										y1=dimensionxy(5,2)
										altosellength'returns fx,fy
										dimensionxy(4,1)=fx
										dimensionxy(4,2)=fy
										linec=linec-1
										'show line between legs
										Line (dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(5,1),dimensionxy(5,2))
										'create line between legs
							'			x1=dimensionxy(4,1)
							'			y1=dimensionxy(4,2)
							'			z1=0
							'			x2=dimensionxy(5,1)
							'			y2=dimensionxy(5,2)
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
										''''''''''
										
										'define arrows
										'define left arrow
										memmanageline
										Dim As Double tempfx1,tempfx2,tempfy1,tempfy2
										lines(linec,1)=dimensionxy(4,1)
										lines(linec,2)=dimensionxy(4,2)
										lines(linec,4)=dimensionxy(5,1)
										lines(linec,5)=dimensionxy(5,2)
										selline=linec
										calcselangle
										selangle=selangle+90
										If selangle>360 Then selangle=selangle-360
										sellength=dimarrowsize/2
										x1=dimensionxy(4,1)
										y1=dimensionxy(4,2)
										altosellength'returns fx,fy
										tempfx1=fx
										tempfy1=fy
										selangle=selangle+180
										If selangle>360 Then selangle=selangle-360
										sellength=dimarrowsize/2
										x1=dimensionxy(4,1)
										y1=dimensionxy(4,2)
										altosellength'returns fx,fy
										tempfx2=fx
										tempfy2=fy
										linec=linec-1
										'show left arrow
										Line (tempfx1,tempfy1)-(tempfx2,tempfy2)
										Line (tempfx1,tempfy1)-(tempdx1,tempdy1)
										Line (tempfx2,tempfy2)-(tempdx1,tempdy1)
										'create left arrow
							'			x1=tempfx1
							'			y1=tempfy1
							'			z1=0
							'			x2=tempfx2
							'			y2=tempfy2
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
							'			
							'			x1=tempfx1
							'			y1=tempfy1
							'			z1=0
							'			x2=tempdx1
							'			y2=tempdy1
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
							'			
							'			x1=tempfx2
							'			y1=tempfy2
							'			z1=0
							'			x2=tempdx1
							'			y2=tempdy1
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
										
										'define right arrow
										memmanageline
										'Dim As Double tempfx1,tempfx2,tempfy1,tempfy2
										lines(linec,1)=dimensionxy(4,1)
										lines(linec,2)=dimensionxy(4,2)
										lines(linec,4)=dimensionxy(5,1)
										lines(linec,5)=dimensionxy(5,2)
										selline=linec
										calcselangle
										selangle=selangle+90
										If selangle>360 Then selangle=selangle-360
										sellength=dimarrowsize/2
										x1=dimensionxy(5,1)
										y1=dimensionxy(5,2)
										altosellength'returns fx,fy
										tempfx1=fx
										tempfy1=fy
										selangle=selangle+180
										If selangle>360 Then selangle=selangle-360
										sellength=dimarrowsize/2
										x1=dimensionxy(5,1)
										y1=dimensionxy(5,2)
										altosellength'returns fx,fy
										tempfx2=fx
										tempfy2=fy
										linec=linec-1
										'show right arrow
										Line (tempfx1,tempfy1)-(tempfx2,tempfy2)
										Line (tempfx1,tempfy1)-(tempdx2,tempdy2)
										Line (tempfx2,tempfy2)-(tempdx2,tempdy2)
										'create right arrow
							'			x1=tempfx1
							'			y1=tempfy1
							'			z1=0
							'			x2=tempfx2
							'			y2=tempfy2
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
							'			
							'			x1=tempfx1
							'			y1=tempfy1
							'			z1=0
							'			x2=tempdx2
							'			y2=tempdy2
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
							'			
							'			x1=tempfx2
							'			y1=tempfy2
							'			z1=0
							'			x2=tempdx2
							'			y2=tempdy2
							'			z2=0
							'			createnewmodlines
							'			lines(linec,8)=blockc+1
							'			lines(linec,9)=blockc+1
										'this is the leader for the dim value
										'no need for this cuz it's a dim of the diameter
	'																dimensionxy(6,1)=mousex
	'																dimensionxy(6,2)=mousey
	'																line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(6,1),dimensionxy(6,2))
										dimvalue=circles(tempselcircle,4)*2
										'tempstring=Str(tempdouble)
										adjustprecision
										'Draw String (mousex,mousey),tempstring
									End If
								Case 76'dimension angle no mouse button is down
									'this is being handled in the create are routines
							End Select
						Case 81 To 83''no mouse buttons are down - not drawing lines - detecting points and modifiying group
							'81 is chamfer
							'82 is fillet
							'83 is best curve fit
							modifying=TRUE
							select case modify
								Case 81'chamfer
									'show the corner being built
									If cpe=TRUE And mouse_clicks=1 Then'cpe (common point exists)
										'If mouse_clicks=1 Or 2 'if needed
										buildchamfer
										'draw line between two lines @ newlength
										Line(flfx,flfy)-(slfx,slfy)
									End If
								Case 82'fillet
									'show the fillet being built
									If cpe=TRUE And mouse_clicks=1 Then'cpe (common point exists)
										buildfillet
										'draw the fillet arc
										Circle (pfxm,pfym),radius,31,arcstart,arcend
									End If
								Case 83'best curve fit
									'show the curve being built
									If reversed2=FALSE And selentity=TRUE And otd="line" And mouse_clicks<>3 Then
										If fourpoints(2,1)=fxm And fourpoints(2,2)=fym Then
											'same point
											'do nothing
										Else
											'this is when the mouse is over the second line
											'that the curve is going to
											'and similar to the first line selected
											'this point on the second line must be
											'the 3rd point of fourpoints
											'and the 4th point of fourpoints
											'must be the other end of this second line
											If fxm=lines(selline,1) And fym=lines(selline,2) Then
												'if fxm,fym is the same as the selline,1 and 2 then
												'fourpoints(2)=lines(selline(1&2))
												fourpoints(3,1)=lines(selline,1)
												fourpoints(3,2)=lines(selline,2)
												'fourpoints(1) is the other end of the line
												fourpoints(4,1)=lines(selline,4)
												fourpoints(4,2)=lines(selline,5)
											Else
												'if fxm,fym is the NOT same as the selline,1 and 2 then
												'fourpoints(2)=lines(selline(4&5))
												fourpoints(3,1)=lines(selline,4)
												fourpoints(3,2)=lines(selline,5)
												'fourpoints(1) is the other end of the line
												fourpoints(4,1)=lines(selline,1)
												fourpoints(4,2)=lines(selline,2)
											End If
											'now that i have 4 points in order
											tempfxm=fxm
											tempfym=fym
											curvesetup
											fxm=tempfxm
											fym=tempfym
										End If
									Else
										If mouse_clicks<>3 Then
											'reversed2=FALSE
											Line(fourpoints(2,1),fourpoints(2,2))-(mousex,mousey)
										End If
										reversed2=FALSE
									End If
									If mouse_clicks>=2 Then
										Line (cmrxcrossx,cmrxcrossy)-(mousex,mousey),3
										mouse_clicks=3
										curvesetup
									End If
							End Select
						Case 109
							If groupscaling=FALSE Then
								modify=0
								modifying=FALSE
								settinggroupscaledown=FALSE
							Else
								'scaling group
								modifying=TRUE
								scalegroup
							End If
						Case 131
							'plotting trajectory
							If modifying=FALSE Then
								modifying=TRUE
								trajix=fxm'modifyx1
								trajiy=fym'modifyy1
								trajiz=0
							EndIf
							plottrajectory
						Case 141
							modify=0
							'selcircle
							'selline
							
							gff=FreeFile
							Open "gcodeout.txt" For Append As #gff
							Select Case otd
								Case "line"
									theboxbelow("gcode: line number="+Str(selline)+crlf)
									If fxm=lines(selline,1) And fym=lines(selline,2) Then
										Print #gff, "G01 X";
										Print #gff, LTrim(Str(lines(selline,1)));
										Print #gff, " Y";
										Print #gff, LTrim(Str(lines(selline,2)))
										Print #gff, "G01 X";
										Print #gff, LTrim(Str(lines(selline,4)));
										Print #gff, " Y";
										Print #gff, LTrim(Str(lines(selline,5)))
									Else
										Print #gff, "G01 X";
										Print #gff, LTrim(Str(lines(selline,4)));
										Print #gff, " Y";
										Print #gff, LTrim(Str(lines(selline,5)))
										Print #gff, "G01 X";
										Print #gff, LTrim(Str(lines(selline,1)));
										Print #gff, " Y";
										Print #gff, LTrim(Str(lines(selline,2)))
									EndIf
								Case "circle"
									theboxbelow("gcode: circle number="+Str(selcircle)+crlf)
								Case "arc"
									theboxbelow("gcode: arc number="+Str(selcircle)+crlf)
								Case "ellipse"
									theboxbelow("gcode: ellipse number="+Str(selcircle)+crlf)
								Case "elliptical arc"
									theboxbelow("gcode: elliptical arc number="+Str(selcircle)+crlf)
									plotellipse_gcode(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
									'theboxbelow("gcode: x="+Str(fxm)+" y="+Str(fym)+crlf)
									If fxm=arcendpoint1x And fym=arcendpoint1y Then
										For i = 1 To gplotc
											'theboxbelow("gcode: x="+Str(gplotx(i))+" y="+Str(gploty(i))+crlf)
											'theboxbelow("G01 X"+LTrim(Str(gplotx(i)))+" Y"+LTrim(Str(gploty(i)))+crlf)
											Print #gff, "G01 X";LTrim(Str(gplotx(i)));" Y";LTrim(Str(gploty(i)))
										Next
										'theboxbelow("gcode: x="+Str(gplotx(1))+" y="+Str(gploty(1))+crlf)
									Else
										For i = gplotc To 1 Step -1
											'theboxbelow("gcode: x="+Str(gplotx(i))+" y="+Str(gploty(i))+crlf)
											'theboxbelow("G01 X"+LTrim(Str(gplotx(i)))+" Y"+LTrim(Str(gploty(i)))+crlf)
											Print #gff, "G01 X";LTrim(Str(gplotx(i)));" Y";LTrim(Str(gploty(i)))
										Next
										'theboxbelow("gcode: x="+Str(gplotx(gplotc))+" y="+Str(gploty(gplotc))+crlf)
									EndIf
							End Select
							Close #gff
							'theboxbelow("gcode: x="+Str(fxm)+" y="+Str(fym)+crlf)
					end select
				end If
			End If
		case 1'left mouse button is down
			if drawingthings=TRUE Then'drawing entities or shapes
				if buttonson(43)=true Then
					if tempmousex<>mousexp or tempmousey<>mouseyp Then pan
				else
					'mouse button is down - drawingthing that take more then two mouse clicks like arcs, ellipses, elliptical arcs and some shapes
					If buttonson(3)=TRUE Or buttonson(4)=TRUE Or buttonson(5)=TRUE Or drawingshapes=TRUE Then
						If buttonson(3)=TRUE Then'arc
							If arcstarted=FALSE Then
								'first click to define arc center
								determinedrawmode1
								arcstarted=TRUE
								mouse_clicks=0
								arcing=FALSE
								arcsetradius=FALSE
							Else
								Select Case mouse_clicks
									Case 1
										If arcsetradius=FALSE Then
											Select Case drawingcirclesmethod
												Case 1
													Select Case shift_key
														Case 0
															
														Case 1,2
															length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
															x1=x1+(x2-x1)/2
															y1=y1+(y2-y1)/2
													End Select
												Case 2
													Select Case shift_key
														Case 1,2
															
														Case 0
															length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
															x1=x1+(x2-x1)/2
															y1=y1+(y2-y1)/2
													End Select
											End Select
											radius=length
											arcsetradius=TRUE
										End If
									Case 2
										If arcing=FALSE Then
											If buttonson(53)=TRUE Then
												If angle2<0 Then angle2=360+angle2
												arcstart=PI*angle2/180
												temparcstart=arcstart
											Else
												arcstart=PI*angle/180
												temparcstart=arcstart
											EndIf
											arcing=TRUE
										EndIf
									Case 3
										determinedrawmode1
								End Select
							EndIf
						End If
						If buttonson(4)=TRUE Then'ellipse
							initarcstart=0
							initarcend=360
							If ellipsing=true Then
								'second click of arcs and ellipses
								If ellipsstarted=false then
									Select Case drawingcirclesmethod
										Case 1
											Select Case shift_key
												Case 0
													
												Case 1,2
													length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
													x1=x1+(x2-x1)/2
													y1=y1+(y2-y1)/2
	
											End Select
										Case 2
											Select Case shift_key
												Case 1,2
													
												Case 0
													length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
													x1=x1+(x2-x1)/2
													y1=y1+(y2-y1)/2
	
											End Select
									End Select
									If buttonson(53)=TRUE Then angle=angle2
									initerotation=angle
									initradius=length
									'arcing=FALSE
									ellipsstarted=TRUE
								End if
							Else
								If ellipsstarted=true Then'3rd click
									 ellipsstarted=FALSE
									 If initradius<eradius Then
									 	swap initradius,eradius
									 	initerotation=initerotation+90
									 EndIf
								EndIf
								erotation=initerotation
								radius=initradius
								arcstart=initarcstart
								arcend=initarcend
								determinedrawmode1
							EndIf
						End If
						If buttonson(5)=TRUE Then'elliptical arc
							If ellipsing=true Then
								'second click of arcs and ellipses
								If ellipsstarted=false Then
									Select Case drawingcirclesmethod
										Case 1
											Select Case shift_key
												Case 0
													
												Case 1,2
													length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
													x1=x1+(x2-x1)/2
													y1=y1+(y2-y1)/2
											End Select
										Case 2
											Select Case shift_key
												Case 1,2
													
												Case 0
													length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
													x1=x1+(x2-x1)/2
													y1=y1+(y2-y1)/2
											End Select
									End Select
									If buttonson(53)=TRUE Then angle=angle2
									initerotation=angle
									initradius=length
									ellipsstarted=TRUE
								End if
							Else
								If ellipsstarted=true Then'3rd click
									'start arcing the ellipse here
									'after second click of arcing then
									'set ellipsstarted to false
									ellipsarcing=TRUE
									If arcing=true Then
										'second click of arc
										If arcstarted=false Then
											Select Case shift_key
												Case 0
													x1p=x2-x1
													y1p=(y2-y1)
													x2=x1p*Cos(-initerotation*d2r) - y1p*Sin(-initerotation*d2r)+x1
													y2=y1p*Cos(-initerotation*d2r) + x1p*Sin(-initerotation*d2r)+y1
													ylength=(y2-y1)*(initradius/eradius)
													xlength=x2-x1
													fixangle
													initarcstart=angle
												Case 1,2
													initarcstart=angle-initerotation
											End Select
	'																	Locate 30,50
	'																	Print arcstart
											arcstarted=TRUE
										End if
									Else
										If arcstarted=true Then
											'fifth click
											erotation=initerotation
											radius=initradius
											eradius=initeradius
											arcstart=initarcstart
											determinedrawmode1
										Else
											If initradius<eradius Then
											 	'90 degree shift
											 	swap initradius,eradius
											 	initerotation=initerotation+90
											EndIf
											initeradius=eradius
										EndIf
									EndIf
								Else
									determinedrawmode1
								EndIf
							EndIf
						End If
						If buttonson(1001)=TRUE Then'Equilateral Triangle
							determinedrawmode1
						EndIf
						'left mouse button is down
						If buttonson(1002)=TRUE Then'Right Triangle
							mouse_clicks+=1
							Select Case mouse_clicks
								Case 1
									determinedrawmode1
								Case 3
									draw_right_triangle
							End Select
						EndIf
						If buttonson(1003)=TRUE Then'Square
							determinedrawmode1
						EndIf
						If buttonson(1004)=TRUE Then'Rectangle
							mouse_clicks+=1
							Select Case mouse_clicks
								Case 1
									determinedrawmode1
								Case 3
									draw_rectangle
							End Select
						EndIf
						If buttonson(1005)=TRUE Then'Rhombus
							mouse_clicks+=1
							Select Case mouse_clicks
								Case 1
									determinedrawmode1
								Case 3
									draw_rhombus
							End Select
						EndIf
						If buttonson(1006)=TRUE Then'Parallelogram
							mouse_clicks+=1
							Select Case mouse_clicks
								Case 1
									determinedrawmode1
								Case 3
									draw_parallelogram
							End Select
						EndIf
						If buttonson(1007)=TRUE Then'Pentagon
							determinedrawmode1
						EndIf
						If buttonson(1008)=TRUE Then'Hexagon
							determinedrawmode1
						EndIf
						If buttonson(1009)=TRUE Then'Heptagon
							determinedrawmode1
						EndIf
						If buttonson(1010)=TRUE Then'Octagon
							determinedrawmode1
						EndIf
					Else
						determinedrawmode1
					EndIf
				End If
			else
				'left mouse button is down
				'not drawing lines - panning or select groups or modifiy group
				if buttonson(43)=true Then
					if tempmousex<>mousexp or tempmousey<>mouseyp Then pan
				Else
					if modify=0 Then
						'no where in if modify=0
						'is modifying set to=true
						for i=31 to 40'copy rotate etc
							if buttonson(i)=true then
								modify=i
								exit for
							end if
						Next
						for i=61 to 62'trim, extend
							if buttonson(i)=true then
								modify=i
								exit for
							end if
						Next
						for i=71 to 80'Dimension
							if buttonson(i)=true Then
								Select Case i
									Case 71 To 73
										modify=i
										dimensioningclicks=1
										exit For
									Case 74
										'Radius Dimension
										If selentity=TRUE Then
											If otd="circle" Or otd="arc" Then
												modify=i
												dimensioningclicks=1
												tempselcircle=selcircle
												exit for
											EndIf
										EndIf
									Case 75
	'																'dimension diameter
										If selentity=TRUE Then
											If otd="circle" Then
												modify=i
												dimensioningclicks=1
												tempselcircle=selcircle
												exit for
											EndIf
										EndIf
									Case 76'left mouse button is down
										'Angle Dimension
										If selentity=TRUE Then
											modify=i
	'																dimensioningclicks=1
											'turn arc on
											buttonson(3)=TRUE
											'turning on button 3 utilized code above for drawing arcs
											'in that code, i added a catch to detect if button 76 is on
											'and if so to do more then just draw an arc but to draw dim arrows etc...
											determinedrawmode1
											arcstarted=TRUE
											mouse_clicks=0
											arcing=FALSE
											arcsetradius=FALSE
											exit For
										End If
								End Select
							end if
						Next
						for i=83 to 83'best fit curving
							'left mouse button is down
							'modify=0 at this point
							'i think mouse_clicks will be zero by default
							'becuase modify is zero at this point
							Select Case i
								Case 83
									If buttonson(i)=true And selentity=TRUE And otd="line" And mouse_clicks=0 Then
										modify=i
										mouse_clicks=1
										'the point that's being clicked here has to be
										'the second point of the four points
										'fxm,fym is the x,y of the point detected
										If fxm=lines(selline,1) And fym=lines(selline,2) Then
											'if fxm,fym is the same as the selline,1 and 2 then
											'fourpoints(2)=lines(selline(1&2))
											fourpoints(2,1)=lines(selline,1)
											fourpoints(2,2)=lines(selline,2)
											'fourpoints(1) is the other end of the line
											fourpoints(1,1)=lines(selline,4)
											fourpoints(1,2)=lines(selline,5)
										Else
											'if fxm,fym is the NOT same as the selline,1 and 2 then
											'fourpoints(2)=lines(selline(4&5))
											fourpoints(2,1)=lines(selline,4)
											fourpoints(2,2)=lines(selline,5)
											'fourpoints(1) is the other end of the line
											fourpoints(1,1)=lines(selline,1)
											fourpoints(1,2)=lines(selline,2)
										End If
										'modifyx1,y1 and modifyx2,y2 - the pivot points
										'used for rotate translate
										'will allways be either fourpoints 1 or 4
										
										
										'curvexy(1,1)=fxm'modifyx1
										'curvexy(1,2)=fym'modifyy1
										'If fxm=lines(selline,1) And fym=lines(selline,2) Then
										'	curvexy(1,3)=selangle+180
										'	If curvexy(1,3)>360 then curvexy(1,3)=curvexy(1,3)-360
										'Else
										'	curvexy(1,3)=selangle
										'EndIf
										'
										''curvexy(1,3)=selangle
										''which line is selected?
										''what is the x,y of the other end of that line
										''this is if the ellipse is rotated
										''this serves as a pivot point for rotate translate
										''so modifyx1,y1 is a pivot point 
										'If lines(selline,1)=fxm Then
										'	modifyx1=lines(selline,4)
										'	modifyy1=lines(selline,5)
										'Else
										'	modifyx1=lines(selline,1)
										'	modifyy1=lines(selline,2)
										'EndIf
									EndIf
							End Select
						Next
						If movingpoints=TRUE Then
							modify=51
							movingpts=true
						EndIf
						if selentity=true Then
							if  modify=0 Then
								tempint=0
								for i=71 to 80'dimensioning or curving
									if buttonson(i)=true then
										tempint=1
										exit for
									end if
								Next
								If tempint=0 Then
									If buttonson(131)=TRUE Then
										modify=131
									Else
										If buttonson(141)=TRUE Then
											modify=141
										Else
											If buttonson(55)=TRUE Then
												modify=55
												fromotd=otd
												Select Case otd
													Case "line"
														perpangle=selangle
														paraselline=selline
													Case "circle","arc"
														paraselcircle=selcircle
														'xlength=fxm-circles(selcircle,1)
														'ylength=fym-circles(selcircle,2)
														'perpangle=atan2(ylength,xlength)*r2d
														'If perpangle<0 then perpangle=360+perpangle
													Case "ellipse","elliptical arc"
														paraselcircle=selcircle
														perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
														If perpangle<0 then perpangle=360+perpangle
												End Select
											Else
												group
												selentity=FALSE
											EndIf
										End If
									End If
								End if
							Else
								modifyx1=fxm
								modifyy1=fym
								modifyz1=fzm
							end if
						Else
							if modify<>0 Then
								modifyx1=mousex
								modifyy1=mousey
								modifyz1=mousez
							Else
								If boxing=true Then
									boxselect=false
									boxselectx2=mousex
									boxselecty2=mousey
									groupbox(0,1)'left mouse button
								Else
									If groupgrab=TRUE Then
										If settinggroupscaledown=FALSE Then
											modify=109
											'Dim Shared As Integer grouplinec, groupcirclec
											'Dim Shared As Integer grouplines(), groupcircles()
											'load selected items into array
											tempgrouplinec=linec+1
											tempgroupcirclec=circlec+1
											grouplinec=0
											For i = 1 To linec
												If lines(i,8)=1 Then
											 		grouplinec=grouplinec+1
												EndIf
											Next
											ReDim grouplines(grouplinec)
											groupcirclec=0
											For i = 1 To circlec
											 	If circles(i,10)=1 Then
											 		groupcirclec=groupcirclec+1
											 	EndIf
											Next
											ReDim groupcircles(groupcirclec)
											
											grouplinec=0
											For i = 1 To linec
												If lines(i,8)=1 Then
											 		grouplinec=grouplinec+1
											 		grouplines(grouplinec)=i
												EndIf
											Next
											groupcirclec=0
											For i = 1 To circlec
											 	If circles(i,10)=1 Then
											 		groupcirclec=groupcirclec+1
											 		groupcircles(groupcirclec)=i
											 	EndIf
											Next
											'now temporarily erase the selected group
											'and create new entities for scaling
											For i = 1 To grouplinec
												memmanageline
												For j=1 To 8
													lines(linec,j)=lines(grouplines(i),j)
												Next
												lines(grouplines(i),8)=-1
											Next
											For i = 1 To groupcirclec
												memmanagecircle
												For j=1 To 11
													circles(circlec,j)=circles(groupcircles(i),j)
												Next
												circles(groupcircles(i),10)=-1
											Next
											groupscaling=TRUE
											'inview()'remove?
											redraw
											'showgroups
										End If
									Else
										tempint=0
										for i=71 to 80'dimensioning' or curving???
											if buttonson(i)=true then
												tempint=1
												exit for
											end if
										Next
										If tempint=0 Then
											If selline=0 And selcircle=0 then
												boxselect=true
												boxselectx1=mousex
												boxselecty1=mousey
											End if
										End If
									End if
								End if
							end if
						end if
						for i=81 to 82'chamfer/fillet
							'left mouse button is down
							'modify=0 at this point
							'i think mouse_clicks will be zero by default
							'becuase modify is zero at this point
							Select Case i
								Case 81,82
									If buttonson(i)=true Then
										'left mouse button is down
										If linesingroupc=2 Then
											cpeforlines(firstlineingroup,secondlineingroup)
											If cpe=TRUE Then
												If cpx=fxm And cpy=fym Then
													modify=i
													mouse_clicks=1
													Exit For
												End If
											Else
												cpe=FALSE
											EndIf
										End If
										'If linesingroupc=2 Then
										'	'do these two line share a common point
										'	flx1=lines(firstlineingroup,1)
										'	fly1=lines(firstlineingroup,2)
										'	flx2=lines(firstlineingroup,4)
										'	fly2=lines(firstlineingroup,5)
										'	slx1=lines(secondlineingroup,1)
										'	sly1=lines(secondlineingroup,2)
										'	slx2=lines(secondlineingroup,4)
										'	sly2=lines(secondlineingroup,5)
										'	cpe=FALSE
										'	If flx1=slx1 And fly1=sly1 Then
										'		cpe=TRUE
										'		cpx=flx1
										'		cpy=fly1
										'		cpfl=1
										'		cpsl=1
										'	EndIf
										'	If flx1=slx2 And fly1=sly2 Then
										'		cpe=TRUE
										'		cpx=flx1
										'		cpy=fly1
										'		cpfl=1
										'		cpsl=2
										'	EndIf
										'	If flx2=slx1 And fly2=sly1 Then
										'		cpe=TRUE
										'		cpx=flx2
										'		cpy=fly2
										'		cpfl=2
										'		cpsl=1
										'	EndIf
										'	If flx2=slx2 And fly2=sly2 Then
										'		cpe=TRUE
										'		cpx=flx2
										'		cpy=fly2
										'		cpfl=2
										'		cpsl=2
										'	EndIf
										'	If cpe=TRUE Then
										'		If cpx=fxm And cpy=fym Then
										'			modify=i
										'			mouse_clicks=1
										'			Exit For
										'		End If
										'	Else
										'		cpe=FALSE
										'	EndIf
										'End If
									EndIf
							End Select
						Next
					else
						if modifying=true Then
							'modifying was set to true after user release left mouse button and moved mouse
							select case modify'set modify
								'set modify to -1 after
								'setmovedown, setcopydown, setrotatedown, setrotatecopydown, setmovepointsdown
								'set modify to 31 after dimensioning in order to prepare to setmovedown
								case 31
									setmovedown
									'screenset 0,0:view:window
									selbutton=31
									turnbuttonoff
									modifying=false
									modify=-1
									If buttonson(76)=TRUE Then
										escapeme
									Else
										redraw
									EndIf
									'redraw
								case 32
									setcopydown
									'screenset 0,0:view:window
									'selbutton=32
									'turnbuttonoff
									'modifying=false
									'modify=-1
									'inview
									'redraw
								case 33
									setrotatedown
									'screenset 0,0:view:window
									selbutton=33
									turnbuttonoff
									modifying=false
									modify=-1
									redraw
								case 34
									setrotatecopydown
									'screenset 0,0:view:window
									'selbutton=34
									'turnbuttonoff
									'modifying=false
									'modify=-1
									'inview
									'redraw
								Case 51
									setmovepointsdown
									movingpts=false
									'screenset 0,0:view:window
									selbutton=51
									turnbuttonoff
									modifying=false
									modify=-1
									redraw
									If splining=TRUE Then spliningmovingpoints=TRUE
								Case 55
									'set parallel down
									Select Case fromotd
										Case "line"
											findallselectedentities
											if selectedentitiesexists=TRUE And selectedentitiestotal>1 Then
												buildselectedentityarrays
												selline=paraselline
												parallelcontour
												paralinesideset=FALSE
												For i = 1 To selectedentitiestotal
													'Print joinedentityorder(i,j);
													'joinedentityorder(i,1)=1 for line 2 for arc
													'joinedentityorder(i,2)=lines(array number) or circles(array number)
													'joinedentityorder(i,3)=1 for line x1,y1 or arcstart 2 for line x2,yx or arcend
													'paraside
													Select Case joinedentityorder(i,1)
														Case 1'line
															selline=joinedentityorder(i,2)
															calcselangle
															perpangle=selangle
															paraselline=selline
															Select Case i
																Case 1
																	paralinesideset=TRUE
																	angle2=perpangle+90
																	aftom
																	pfxm=fx
																	pfym=fy
																	angle2=perpangle-90
																	aftom
																	If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then
																		angle2=perpangle+90
																		Select Case joinedentityorder(i,3)
																			Case 1
																				paraside=TRUE
																			Case 2
																				paraside=FALSE
																		End Select
																	Else
																		angle2=perpangle-90
																		Select Case joinedentityorder(i,3)
																			Case 1
																				paraside=FALSE
																			Case 2
																				paraside=TRUE
																		End Select
																	EndIf
																Case Else
																	If paralinesideset=TRUE Then
																		Select Case joinedentityorder(i,3)
																			Case 1
																				Select Case paraside
																					Case TRUE
																						angle2=perpangle+90
																					Case FALSE
																						angle2=perpangle-90
																				End Select
																			Case 2
																				Select Case paraside
																					Case TRUE
																						angle2=perpangle-90
																					Case FALSE
																						angle2=perpangle+90
																				End Select
																		End Select
																	Else
																		'actually this wont be the case
																		'because this routine is only if
																		'user has in fact first selected
																		'a line to start the parallel
																		'but the idea of needing new fxm,fym/mousex,mousey from arc point 2 still applies
																		'first in was an arc
																		'paraside still needs to be defined
																		paralinesideset=TRUE
																		'this now needs new fxm,fym and mousex,mousey
																		'this data should come from last arc point 2
																		angle2=perpangle+90
																		aftom
																		pfxm=fx
																		pfym=fy
																		angle2=perpangle-90
																		aftom
																		If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then
																			angle2=perpangle+90
																			Select Case joinedentityorder(i,3)
																				Case 1
																					paraside=TRUE
																				Case 2
																					paraside=FALSE
																			End Select
																		Else
																			angle2=perpangle-90
																			Select Case joinedentityorder(i,3)
																				Case 1
																					paraside=FALSE
																				Case 2
																					paraside=TRUE
																			End Select
																		EndIf
																	End If
															End Select
															
															x1=lines(paraselline,1)+cos(angle2*d2r)*para_offset
															y1=lines(paraselline,2)+sin(angle2*d2r)*para_offset
															x2=lines(paraselline,4)+cos(angle2*d2r)*para_offset
															y2=lines(paraselline,5)+sin(angle2*d2r)*para_offset
															'lc=lines(paraselline,7)
															lc=15
															createnewmodlines()
															joinedentityorder(i,4)=joinedentityorder(i,1)
															joinedentityorder(i,5)=linec
															joinedentityorder(i,6)=joinedentityorder(i,3)
															
															Select Case joinedentityorder(i,3)
																Case 1
																	mousex=x2
																	mousey=y2
																Case 2
																	mousex=x1
																	mousey=y1
															End Select
														Case 2'arc
															'selline=joinedentityorder(i,2)
															'calcselangle
															'perpangle=selangle
															paraselcircle=joinedentityorder(i,2)
															para_radius=circles(paraselcircle,4)
															Select Case calcd(mousex,mousey,0,circles(paraselcircle,1),circles(paraselcircle,2),0)
																Case Is < para_radius
																	para_radius=para_radius-para_offset
																Case Else
																	para_radius=para_radius+para_offset
															End Select
															'Select Case circles(paraselcircle,9)
															'	Case 1
															'		circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5)
															'	Case 2
															'		circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5),circles(paraselcircle,6),circles(paraselcircle,7)
															'	Case 3,4
															'		plotellipse(circles(paraselcircle,1),circles(paraselcircle,2),circles(paraselcircle,3),para_radius,Int(circles(paraselcircle,5)),circles(paraselcircle,6),circles(paraselcircle,7),circles(paraselcircle,8),circles(paraselcircle,11))
															'End Select
															memmanagecircle
															For j = 1 To 11
																circles(circlec,j)=circles(paraselcircle,j)
															next
															circles(circlec,4)=para_radius
															circles(circlec,5)=lc
															circles(circlec,10)=0
															calcarcendpoints2(circlec)
															joinedentityorder(i,4)=joinedentityorder(i,1)
															joinedentityorder(i,5)=circlec
															joinedentityorder(i,6)=joinedentityorder(i,3)
															Select Case joinedentityorder(i,3)
																Case 1
																	mousex=arcendpoint2x
																	mousey=arcendpoint2y
																Case 2
																	mousex=arcendpoint1x
																	mousey=arcendpoint1y
															End Select
													End Select
												Next
											Else
												'no group - single entity
												angle2=perpangle+90
												aftom
												pfxm=fx
												pfym=fy
												angle2=perpangle-90
												aftom
												If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then angle2=perpangle+90
												x1=lines(paraselline,1)+cos(angle2*d2r)*para_offset
												y1=lines(paraselline,2)+sin(angle2*d2r)*para_offset
												x2=lines(paraselline,4)+cos(angle2*d2r)*para_offset
												y2=lines(paraselline,5)+sin(angle2*d2r)*para_offset
												'lc=lines(paraselline,7)
												createnewmodlines()
											EndIf
											'modifying=false
											'modify=-1
											'inview
											'redraw
											
												'For i=1 to selectedentitiestotal
												'	Print i;
												'	For j=1 To 3
												'		Print joinedentityorder(i,j);
												'	Next
												'	print
												'Next
										Case "circle","arc","ellipse","elliptical arc"
											findallselectedentities
											if selectedentitiesexists=TRUE And selectedentitiestotal>1 Then
												buildselectedentityarrays
												selcircle=paraselcircle
												parallelcontour
												paralinesideset=FALSE
												For i = 1 To selectedentitiestotal
													'Print joinedentityorder(i,j);
													'joinedentityorder(i,1)=1 for line 2 for arc
													'joinedentityorder(i,2)=lines(array number) or circles(array number)
													'joinedentityorder(i,3)=1 for line x1,y1 or arcstart 2 for line x2,yx or arcend
													'paraside
													Select Case joinedentityorder(i,1)
														Case 1'line
															selline=joinedentityorder(i,2)
															calcselangle
															perpangle=selangle
															paraselline=selline
															Select Case i
																Case 1
																	paralinesideset=TRUE
																	angle2=perpangle+90
																	aftom
																	pfxm=fx
																	pfym=fy
																	angle2=perpangle-90
																	aftom
																	If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then
																		angle2=perpangle+90
																		Select Case joinedentityorder(i,3)
																			Case 1
																				paraside=TRUE
																			Case 2
																				paraside=FALSE
																		End Select
																	Else
																		angle2=perpangle-90
																		Select Case joinedentityorder(i,3)
																			Case 1
																				paraside=FALSE
																			Case 2
																				paraside=TRUE
																		End Select
																	EndIf
																Case Else
																	If paralinesideset=TRUE Then
																		Select Case joinedentityorder(i,3)
																			Case 1
																				Select Case paraside
																					Case TRUE
																						angle2=perpangle+90
																					Case FALSE
																						angle2=perpangle-90
																				End Select
																			Case 2
																				Select Case paraside
																					Case TRUE
																						angle2=perpangle-90
																					Case FALSE
																						angle2=perpangle+90
																				End Select
																		End Select
																	Else
																		'actually this wont be the case
																		'because this routine is only if
																		'user has in fact first selected
																		'a line to start the parallel
																		'but the idea of needing new fxm,fym/mousex,mousey from arc point 2 still applies
																		'first in was an arc
																		'paraside still needs to be defined
																		paralinesideset=TRUE
																		'this now needs new fxm,fym and mousex,mousey
																		'this data should come from last arc point 2
																		Select Case joinedentityorder(i,3)
																			Case 1
																				fxm=lines(joinedentityorder(i,2),1)
																				fym=lines(joinedentityorder(i,2),2)
																			Case 2
																				fxm=lines(joinedentityorder(i,2),4)
																				fym=lines(joinedentityorder(i,2),5)
																		End Select
																		angle2=perpangle+90
																		aftom
																		pfxm=fx
																		pfym=fy
																		angle2=perpangle-90
																		aftom
																		If calcd(mousex,mousey,0,pfxm,pfym,0)<calcd(mousex,mousey,0,fx,fy,0) Then
																			angle2=perpangle+90
																			Select Case joinedentityorder(i,3)
																				Case 1
																					paraside=TRUE
																				Case 2
																					paraside=FALSE
																			End Select
																		Else
																			angle2=perpangle-90
																			Select Case joinedentityorder(i,3)
																				Case 1
																					paraside=FALSE
																				Case 2
																					paraside=TRUE
																			End Select
																		EndIf
																	End If
															End Select
															
															x1=lines(paraselline,1)+cos(angle2*d2r)*para_offset
															y1=lines(paraselline,2)+sin(angle2*d2r)*para_offset
															x2=lines(paraselline,4)+cos(angle2*d2r)*para_offset
															y2=lines(paraselline,5)+sin(angle2*d2r)*para_offset
															'lc=lines(paraselline,7)
															lc=15
															createnewmodlines()
															joinedentityorder(i,4)=joinedentityorder(i,1)
															joinedentityorder(i,5)=linec
															joinedentityorder(i,6)=joinedentityorder(i,3)
															Select Case joinedentityorder(i,3)
																Case 1
																	mousex=x2
																	mousey=y2
																Case 2
																	mousex=x1
																	mousey=y1
															End Select
														Case 2'arc
															'selline=joinedentityorder(i,2)
															'calcselangle
															'perpangle=selangle
															paraselcircle=joinedentityorder(i,2)
															para_radius=circles(paraselcircle,4)
															Select Case calcd(mousex,mousey,0,circles(paraselcircle,1),circles(paraselcircle,2),0)
																Case Is < para_radius
																	para_radius=para_radius-para_offset
																Case Else
																	para_radius=para_radius+para_offset
															End Select
															'Select Case circles(paraselcircle,9)
															'	Case 1
															'		circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5)
															'	Case 2
															'		circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5),circles(paraselcircle,6),circles(paraselcircle,7)
															'	Case 3,4
															'		plotellipse(circles(paraselcircle,1),circles(paraselcircle,2),circles(paraselcircle,3),para_radius,Int(circles(paraselcircle,5)),circles(paraselcircle,6),circles(paraselcircle,7),circles(paraselcircle,8),circles(paraselcircle,11))
															'End Select
															memmanagecircle
															For j = 1 To 11
																circles(circlec,j)=circles(paraselcircle,j)
															next
															circles(circlec,4)=para_radius
															circles(circlec,5)=lc
															circles(circlec,10)=0
															calcarcendpoints2(circlec)
															joinedentityorder(i,4)=joinedentityorder(i,1)
															joinedentityorder(i,5)=circlec
															joinedentityorder(i,6)=joinedentityorder(i,3)
															Select Case joinedentityorder(i,3)
																Case 1
																	mousex=arcendpoint2x
																	mousey=arcendpoint2y
																Case 2
																	mousex=arcendpoint1x
																	mousey=arcendpoint1y
															End Select
													End Select
												Next
											Else
												'no group - create single parallel to an arc
												para_radius=circles(paraselcircle,4)
												Select Case calcd(mousex,mousey,0,circles(paraselcircle,1),circles(paraselcircle,2),0)
													Case Is < para_radius
														para_radius=para_radius-para_offset
													Case Else
														para_radius=para_radius+para_offset
												End Select
												'Select Case circles(paraselcircle,9)
												'	Case 1
												'		circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5)
												'	Case 2
												'		circle(circles(paraselcircle,1),circles(paraselcircle,2)),para_radius,circles(paraselcircle,5),circles(paraselcircle,6),circles(paraselcircle,7)
												'	Case 3,4
												'		plotellipse(circles(paraselcircle,1),circles(paraselcircle,2),circles(paraselcircle,3),para_radius,Int(circles(paraselcircle,5)),circles(paraselcircle,6),circles(paraselcircle,7),circles(paraselcircle,8),circles(paraselcircle,11))
												'End Select
												memmanagecircle
												For j = 1 To 11
													circles(circlec,j)=circles(paraselcircle,j)
												next
												circles(circlec,4)=para_radius
												circles(circlec,5)=lc
												circles(circlec,10)=0
											EndIf
									End Select
									'now fix corners
									'depending on user preference
									'build arc between two paralines
									'or extend paralines to each other
									'for inside coners, trim them
									For i = 2 To selectedentitiestotal
										Select Case joinedentityorder(i,4)
											Case 1'lines
												If joinedentityorder(i-1,4)=1 Then'another line
													'calc intersection
													aintersect=joinedentityorder(i-1,5)
													bintersect=joinedentityorder(i,5)
													calclinelineintersection
													If intersection="outside" And buttonson(66)=TRUE Then
														'create an arc
														Select Case joinedentityorder(i,3)
															Case 1
																x1=lines(joinedentityorder(i,2),1)
																y1=lines(joinedentityorder(i,2),2)
																arcstart=abtp(x1,y1,0,lines(joinedentityorder(i,5),1),lines(joinedentityorder(i,5),2),0)
															Case 2
																x1=lines(joinedentityorder(i,2),4)
																y1=lines(joinedentityorder(i,2),5)
																arcstart=abtp(x1,y1,0,lines(joinedentityorder(i,5),4),lines(joinedentityorder(i,5),5),0)
														End Select
														Select Case joinedentityorder(i-1,3)
															Case 1
																arcend=abtp(x1,y1,0,lines(joinedentityorder(i-1,5),4),lines(joinedentityorder(i-1,5),5),0)
															Case 2
																arcend=abtp(x1,y1,0,lines(joinedentityorder(i-1,5),1),lines(joinedentityorder(i-1,5),2),0)
														End Select
														'going from arcstat to arcend in a counter clockwise direction
														'the angle from x1,y1 to fxm,fym should be in the middle
														memmanagecircle
														circles(circlec,1)=x1
														circles(circlec,2)=y1
														circles(circlec,4)=para_offset'radius
														circles(circlec,5)=lc
														circles(circlec,6)=arcstart*d2r
														circles(circlec,7)=arcend*d2r
														circles(circlec,9)=2
														xlength=fxm-x1
														ylength=fym-y1
														fixangle
														fx=x1+cos(angle*d2r)*para_offset
														fy=y1+sin(angle*d2r)*para_offset
														calcarcmidpoint2(circlec)
														If calcd(_
															fx,_
															fy,_
															0,_
															arcmidpointx,_
															arcmidpointy,_
															0_
																) > jointolerance Then
															Swap circles(circlec,6),circles(circlec,7)
														End If
													EndIf
													If intersection="inside" Then
														Select Case joinedentityorder(i-1,6)
															Case 1
																lines(aintersect,4)=fxm
																lines(aintersect,5)=fym
															Case 2
																lines(aintersect,1)=fxm
																lines(aintersect,2)=fym
														End Select
														Select Case joinedentityorder(i,6)
															Case 1
																lines(bintersect,1)=fxm
																lines(bintersect,2)=fym
															Case 2
																lines(bintersect,4)=fxm
																lines(bintersect,5)=fym
														End Select
													Else
														If buttonson(65)=TRUE Then
															Select Case joinedentityorder(i-1,6)
																Case 1
																	lines(aintersect,4)=fxm
																	lines(aintersect,5)=fym
																Case 2
																	lines(aintersect,1)=fxm
																	lines(aintersect,2)=fym
															End Select
															Select Case joinedentityorder(i,6)
																Case 1
																	lines(bintersect,1)=fxm
																	lines(bintersect,2)=fym
																Case 2
																	lines(bintersect,4)=fxm
																	lines(bintersect,5)=fym
															End Select
														EndIf
													End If
												EndIf
												If i=selectedentitiestotal And entitycircuit="closed" Then'check this last entity with first entity
													If joinedentityorder(1,4)=1 Then'first entity is a line
														'calc intersection
														aintersect=joinedentityorder(i,5)'previous
														bintersect=joinedentityorder(1,5)'this one
														calclinelineintersection
														If intersection="outside" And buttonson(66)=TRUE Then
															'create an arc
															Select Case joinedentityorder(1,3)
																Case 1
																	x1=lines(joinedentityorder(1,2),1)
																	y1=lines(joinedentityorder(1,2),2)
																	arcstart=abtp(x1,y1,0,lines(joinedentityorder(1,5),1),lines(joinedentityorder(1,5),2),0)
																Case 2
																	x1=lines(joinedentityorder(1,2),4)
																	y1=lines(joinedentityorder(1,2),5)
																	arcstart=abtp(x1,y1,0,lines(joinedentityorder(1,5),4),lines(joinedentityorder(1,5),5),0)
															End Select
															Select Case joinedentityorder(i,3)
																Case 1
																	arcend=abtp(x1,y1,0,lines(joinedentityorder(i,5),4),lines(joinedentityorder(i,5),5),0)
																Case 2
																	arcend=abtp(x1,y1,0,lines(joinedentityorder(i,5),1),lines(joinedentityorder(i,5),2),0)
															End Select
															'going from arcstat to arcend in a counter clockwise direction
															'the angle from x1,y1 to fxm,fym should be in the middle
															memmanagecircle
															circles(circlec,1)=x1
															circles(circlec,2)=y1
															circles(circlec,4)=para_offset'radius
															circles(circlec,5)=lc
															circles(circlec,6)=arcstart*d2r
															circles(circlec,7)=arcend*d2r
															circles(circlec,9)=2
															xlength=fxm-x1
															ylength=fym-y1
															fixangle
															fx=x1+cos(angle*d2r)*para_offset
															fy=y1+sin(angle*d2r)*para_offset
															calcarcmidpoint2(circlec)
															If calcd(_
																fx,_
																fy,_
																0,_
																arcmidpointx,_
																arcmidpointy,_
																0_
																	) > jointolerance Then
																Swap circles(circlec,6),circles(circlec,7)
															End If
														EndIf
														If intersection="inside" Then
															Select Case joinedentityorder(i,6)
																Case 1
																	lines(aintersect,4)=fxm
																	lines(aintersect,5)=fym
																Case 2
																	lines(aintersect,1)=fxm
																	lines(aintersect,2)=fym
															End Select
															Select Case joinedentityorder(1,6)
																Case 1
																	lines(bintersect,1)=fxm
																	lines(bintersect,2)=fym
																Case 2
																	lines(bintersect,4)=fxm
																	lines(bintersect,5)=fym
															End Select
														Else
															If buttonson(65)=TRUE Then
																Select Case joinedentityorder(i,6)
																	Case 1
																		lines(aintersect,4)=fxm
																		lines(aintersect,5)=fym
																	Case 2
																		lines(aintersect,1)=fxm
																		lines(aintersect,2)=fym
																End Select
																Select Case joinedentityorder(1,6)
																	Case 1
																		lines(bintersect,1)=fxm
																		lines(bintersect,2)=fym
																	Case 2
																		lines(bintersect,4)=fxm
																		lines(bintersect,5)=fym
																End Select
															EndIf
														End If
													EndIf
												EndIf
											Case 2'arcs
												theboxbelow("next is arc")
										End Select
									Next
									modifying=false
									modify=-1
									inview()'don't remove
									'cuz new entities created need to be added to view
									redraw
								Case 71 To 73
									If dimensioningclicks=3 then
										setdimensiondown
									Else'second click
										dimensioningclicks=2
									End If
								Case 74 To 75
									'76 is handled in determinedrawmode1 for some reason
									'dimension radius
									setdimensiondown
								Case 81 To 83
									'81 is chamfer
									'82 is fillet
									'83 is best curve fit
									'
									'delete the two lines and create two new lines
									'deleting / creating is for purpose of undo
									'but what if the line is part of a block???
									'do i explode the block(s) the two lines belong to???
									'lines(linec,8)=-1
									'lines(linec,9)=blockc
									'see "delete" in section for pressing the delete key on kybd
									'for an example of addressing exploding blocks
									Select Case modify
										Case 81,82
											'turn chamfer off
											'screenset 0,0:view:window
											'selbutton=modify				
											'turnbuttonoff
											'if needed select case mouse_clicks
											trimlinesincf(firstlineingroup,secondlineingroup)
										Case 83
											'obviously i forgot to think about the idea
											'of creating a best fit curve between two lines
											'if the lines were part of a block
									End Select
									'this is for creating the chamfer/fillet
									Select Case modify
										Case 81
											'here is where to set chamfer down
											'if needed select case mouse_clicks
											'note curving clicks=1
											setchamferdown(firstlineingroup,secondlineingroup)
											modifying=false
											modify=-1
											inview()'don't remove
											'cuz new entities created need to be added to view
											redraw
										Case 82
											'here is where to set fillet down
											'if needed select case mouse_clicks
											'note curving clicks=1
											setfilletdown(firstlineingroup,secondlineingroup)
											modifying=false
											modify=-1
											inview()'don't remove
											'cuz new entities created need to be added to view
											redraw
										Case 83
											Select Case mouse_clicks
												'where is second click???
												'If cmrxcross=TRUE Then mouse_clicks=2
												'so how's it get from 2 to 3
												'must be when mouse is released
												Case 1
													If selentity=TRUE And otd="line" Then
														'plottheellipse
														'curvexy(2,1)=fxm'modifyx2
														'curvexy(2,2)=fym'modifyy2
														'curvexy(2,3)=selangle
														'here is where i would calculate and create the curve
														'setcurvedown
														'instead of setcurvedown
														'just set setsplinedown to true
														'and call curvesetup
														If cmrxcross=TRUE Then
															mouse_clicks=2
														Else
															mouse_clicks=0
															setsplinedown=TRUE
															curvesetup
															setsplinedown=FALSE
															'screenset 0,0:view:window
															modifying=false
															modify=-1
															redraw
														End If
													End If
												Case 3
													'set the curve down
													setsplinedown=TRUE
													curvesetup
													setsplinedown=FALSE
													mouse_clicks=0
													'screenset 0,0:view:window
													modifying=false
													modify=-1
													redraw
											End Select
									End Select
								Case 109
									'set resized group down
									If settinggroupscaledown=FALSE Then
										groupscaling=FALSE
										groupgrab=FALSE
										scalegroup
										settinggroupscaledown=TRUE
										'delete the temp entities
										'set the original group back to being selected
										'scale this group accordingly
									End if
							end Select
						Else
							'if user is trying to scale group
							'and still holding the left mouse button down
							'without having released the left mouse button
							'then do the same thing as that of if the user
							'had clicked and released the left mouse button
							If groupscaling=TRUE Then scalegroup
						end if
					end If
				End If
			end if
		case 2'right mouse button is down
			if drawmode=true then'get out of line drawing mode
				If splining=TRUE Then setsplinedown=TRUE
				escapeme
			Else
				if modifying=true Then
					mouse_clicks=0
					movingpts=false
					modifying=FALSE
					If modify=31 Then
						For i = 1 to linec
							if lines(i,8)=2 then lines(i,8)=1
						Next
						For i = 1 To circlec
							if circles(i,10)=2 Then circles(i,10)=1
						Next
					EndIf
					modify=0
					'degroup
					redraw
					'screenset 0,0:view:window
					for i = 31 to 40
						selbutton=i
						turnbuttonoff
					Next
				Else
					drawatangle=FALSE
					forcex=FALSE
					forcey=FALSE
					If boxing=true Then
						boxing=false
						boxselect=false
						boxselectx2=mousex
						boxselecty2=mousey
						groupbox(1,0)'right mouse button
					Else
						If selentity=TRUE Then
							insertionx=fxm
							insertiony=fym
							'insertionz=fzy
							theboxbelow("insertion Point set")
						Else
							insertionx=mousex
							insertiony=mousey
							'insertionz=fzy
						End If
					End if
				end If
			End if
		case 4'mousewheel is pressed down
	End Select
	If drawing=TRUE Then
		If tempmousex<>mousexp or tempmousey<>mouseyp Then
			trackangle
			tracklength
		End If
	End If
	
	'commented out is a section of code that goes here.
	'it is a bunch of text to display on the screen as things are happening
	
	'mousex=tempmousex
	'mousey=tempmousey
	'mousexp=mousex
	'mouseyp=mousey

	mousexp=tempmousex
	mouseyp=tempmousey
	'mouseb=0
	if buttonson(43)<>true Then mouseb=0
End Sub
sub initlinedraw()
	'If buttonson(22)=TRUE Then
	'	'i think drawatangle
	'	'forcex and y are to rermain unchange
	'	forcex=TRUE
	'	forcey=TRUE
	'Else
	'	forcelength=FALSE
	'	drawatangle=false
	'	forcex=false
	'	forcey=FALSE
	'End If
	forcelength=FALSE
	drawatangle=false
	forcex=false
	forcey=FALSE
	forcedx=FALSE
	forcedy=FALSE
	ellipsing=false
	ellipsstarted=false
	ellipsarcing=false
	arcing=false
	arcstarted=FALSE
	inview()'dont remove new entity created
	redraw
End Sub
sub csntodec()
	If InStr(tempstring,"e-") Then
		If Mid$(tempstring,1,1)="-" Then
			tempstring = "-0." + String$ (Val(Mid$(tempstring,InStr(tempstring,"e-")+2))-1,"0") + Mid$(tempstring,2,1) + Mid$(tempstring,4,InStr(tempstring,"e-")-4)
		Else
			tempstring = "0." + String$ (Val(Mid$(tempstring,InStr(tempstring,"e-")+2))-1,"0") + Mid$(tempstring,1,1) + Mid$(tempstring,3,InStr(tempstring,"e-")-4)
		EndIf
	End if
End Sub
Sub savedxf(saveasfilename As String)
	Dim As Integer i,j,k,entityc,entityt,dxfblocki
	If rayenabled=TRUE Then
		For i = rayi To rayi+7
			lines(i,8)=-1
		Next
	End If
	dxfhandlec=101
	entityc=101'this is preset based on pre handle count
	'i like this dxf file as my template because it has the
	'15 layers preceeding the blocks and entities
	'but for some reason the handle for the first line or entity
	'is before the handles for the layers
	'then the handles for the rest of the entities continues
	'after the layers and it is due to this that i am placing a dummie
	'entity (a line from 0,0 to 0,0) in it's place
	'so that i can the increment my entity handlse starting after
	'the layer handles... i think i might need a couple other dummie
	'handlse for other things like block records... we'll see. 
	entityt=entityc
	'for each entity that is not part of a block
	'increment entityt
	If savedxfblockonly=TRUE Then
		For i = 1 To linec
			if lines(i,8)>=0 And lines(i,9)=savedxfblockonlyi then entityt=entityt+1
		Next
		For i = 1 To circlec
			If circles(i,10)>=0 And circles(i,12)=savedxfblockonlyi then entityt=entityt+1
		Next
	Else
		For i = 1 To linec
			if lines(i,8)>=0 And lines(i,9)=0 then entityt=entityt+1
		Next
		For i = 1 To circlec
			If circles(i,10)>=0 And circles(i,12)=0 then entityt=entityt+1
		Next
		
		Dim dupeblocks(blockc) As String
		Dim dupeblocksc As Integer
		Dim As BOOLEAN activeblock, dupeblock
		dupeblocksc=0
		For j=1 To blockc
			activeblock=FALSE
			For i = 1 To linec
				if lines(i,8)>=0 And lines(i,9)=j Then
					dupeblock=FALSE
					For k=1 To dupeblocksc
						If dupeblocks(k)=blocknames(j) Then
							dupeblock=TRUE
							Exit for
						EndIf
					Next
					If dupeblock=FALSE Then
						activeblock=TRUE
						entityt=entityt+1
					End If
				End If
			Next
			For i = 1 To circlec
				if circles(i,10)>=0 And circles(i,12)=j Then
					dupeblock=FALSE
					For k=1 To dupeblocksc
						If dupeblocks(k)=blocknames(j) Then
							dupeblock=TRUE
							Exit for
						EndIf
					Next
					If dupeblock=FALSE Then
						activeblock=TRUE
						entityt=entityt+1
					End If
				End If
			Next
			'each active block uses 3 handles
			'D7 is in the tables section
			'D8 and D9 are in the block section
			If activeblock=TRUE Then
				createdxfblockdata=TRUE
				dupeblocksc=dupeblocksc+1
				dupeblocks(dupeblocksc)=blocknames(j)
				'because there is an active block
				entityt=entityt+1'for the insert of the block in the entities section
				If dupeblock=FALSE Then
					entityt=entityt+3'for ? in ? section (tables???)
				End If
			EndIf
		Next
	End If
	'in case i impliment more then my 15 layers
'	For i = 1 To number of layers (15)
'		entityt=entityt+1
'	Next
'	Or just increase by 15
	entityt=entityt+15
	
	dxfhandlet=entityt
	If savedxfblockonly=TRUE Then
		tempstring=blockname+".dxf"
		open tempstring for output as #1
	Else
		open saveasfilename for output as #1
	EndIf
	
	dxfheader1'active dxf header
	dxfheader2'inactive dxf header cont.
	Print #1, "  9"
	Print #1, "$HANDSEED"
	Print #1, "  5"
	Print #1, Hex$(dxfhandlet+1)'"EF"'this hex value is one more then last handle vaule
	dxfheader3'inactive dxf header cont.
	Print #1, "  0"
	Print #1, "ENDSEC"
	dxfclasses'inactive dxf classes
	Print #1, "  0"
	Print #1, "SECTION"
	Print #1, "  2"
	Print #1, "TABLES"
	dxftablevport'active with handles dxf tables - vports
	'has a lot of uncommented (active stuff in it)
	
	dxftableltype'active with handles dxf tables - LTYPE
	dxftablelayers'active with handles dxf tables - LAYERS
	'mod this and also mod this if adding new layers
	dxftableviewucsappiddimstyle'active with handles dxf tables
	' - VIEW, UCS, APPID, and DIMSTYLE
	
	'originaldxfstuff 8,9 and 10 are for blocks
	'pay attention to if there are any blocks to create or not
	
	'for block table records i need extra per block data?
	dxftableblocks'active with handles dxf tables - blocks
	Print #1, "  0"
	Print #1, "ENDSEC"
	dxfblockpreamble'dxf block section preamble
	'here in the block section do this for each block
	'dxfbuildblocks is called as a sub routine
	
	'this closes out the block section
	dxfentities'dxf entities section
	If savedxfblockonly=TRUE Then
		Close #3
	Else 
		Close #2
		Close #3
		flipellipseaxis("fbcadtempdxfblockdata.dat")
		Open "fbcadtempdxfblockdata.dat" For Input As #2
		Do While Not Eof(2)
			Line Input #2, tempstring
			Print #1, tempstring
		Loop
		Close #2
	End If
	Print #1, "  0"
	Print #1, "ENDSEC"
	
	'now the entity section if there are any enties aside from blocks
	Print #1, "  0"
	Print #1, "SECTION"
	Print #1, "  2"
	Print #1, "ENTITIES"
	flipellipseaxis("fbcadtempdxfentitydata.dat")
	Open "fbcadtempdxfentitydata.dat" For Input As #2
	Do While Not Eof(2)
		Line Input #2, tempstring
		Print #1, tempstring
	Loop
	Close #2
	Print #1, "  0"
	Print #1, "ENDSEC"
	
	dxfdictionarys
	
	Print #1, "  0"
	Print #1, "EOF"
	Close #1
	
	'flipellipseaxis
	If rayenabled=TRUE Then
		For i = rayi To rayi+7
			lines(i,8)=0
		Next
	End If
End Sub
Sub dxfheader1()'active
	Print #1, "  0"
	Print #1, "SECTION"
	Print #1, "  2"
	Print #1, "HEADER"
	Print #1, "  9"
	Print #1, "$ACADVER"
	Print #1, "  1"
	Print #1, "AC1012"
	Print #1, "  9"
	Print #1, "$DWGCODEPAGE"
	Print #1, "  3"
	Print #1, "ANSI_1252"
End Sub
Sub dxfheader2()'inactive dxf header cont.
'	Print #1, "  9"
'	Print #1, "$INSBASE"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$EXTMIN"
'	Print #1, " 10"
'	Print #1, "-4.792595508409931"
'	Print #1, " 20"
'	Print #1, "-0.0052679071561954"
'	Print #1, " 30"
'	Print #1, "-0.0000000100019835"
'	Print #1, "  9"
'	Print #1, "$EXTMAX"
'	Print #1, " 10"
'	Print #1, "104.792596528908"
'	Print #1, " 20"
'	Print #1, "100.0052690513378"
'	Print #1, " 30"
'	Print #1, "0.0000000100019837"
	'my mod
	'actually it seems like view port setting over ride extminmax
'	Print #1, "  9"
'	Print #1, "$EXTMIN"
'	Print #1, " 10"
'	Print #1, LTrim(Str(extentsx1))
'	Print #1, " 20"
'	Print #1, LTrim(Str(extentsy1))
'	Print #1, " 30"
'	Print #1, "0"
'	Print #1, "  9"
'	Print #1, "$EXTMAX"
'	Print #1, " 10"
'	Print #1, LTrim(Str(extentsx2))
'	Print #1, " 20"
'	Print #1, LTrim(Str(extentsy2))
'	Print #1, " 30"
'	Print #1, "0"
	'original stuff
'	Print #1, "  9"
'	Print #1, "$LIMMIN"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$LIMMAX"
'	Print #1, " 10"
'	Print #1, "12.0"
'	Print #1, " 20"
'	Print #1, "9.0"
'	Print #1, "  9"
'	Print #1, "$ORTHOMODE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$REGENMODE"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$FILLMODE"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$QTEXTMODE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$MIRRTEXT"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DRAGMODE"
'	Print #1, " 70"
'	Print #1, "     2"
'	Print #1, "  9"
'	Print #1, "$LTSCALE"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$OSMODE"
'	Print #1, " 70"
'	Print #1, "    37"
'	Print #1, "  9"
'	Print #1, "$ATTMODE"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$TEXTSIZE"
'	Print #1, " 40"
'	Print #1, "0.2"
'	Print #1, "  9"
'	Print #1, "$TRACEWID"
'	Print #1, " 40"
'	Print #1, "0.05"
'	Print #1, "  9"
'	Print #1, "$TEXTSTYLE"
'	Print #1, "  7"
'	Print #1, "STANDARD"
'	Print #1, "  9"
'	Print #1, "$CLAYER"
'	Print #1, "  8"
'	Print #1, "MYLAYER9"
'	Print #1, "  9"
'	Print #1, "$CELTYPE"
'	Print #1, "  6"
'	Print #1, "BYLAYER"
'	Print #1, "  9"
'	Print #1, "$CECOLOR"
'	Print #1, " 62"
'	Print #1, "   256"
'	Print #1, "  9"
'	Print #1, "$CELTSCALE"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$DELOBJ"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DISPSILH"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMSCALE"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$DIMASZ"
'	Print #1, " 40"
'	Print #1, "0.18"
'	Print #1, "  9"
'	Print #1, "$DIMEXO"
'	Print #1, " 40"
'	Print #1, "0.0625"
'	Print #1, "  9"
'	Print #1, "$DIMDLI"
'	Print #1, " 40"
'	Print #1, "0.38"
'	Print #1, "  9"
'	Print #1, "$DIMRND"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$DIMDLE"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$DIMEXE"
'	Print #1, " 40"
'	Print #1, "0.18"
'	Print #1, "  9"
'	Print #1, "$DIMTP"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$DIMTM"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$DIMTXT"
'	Print #1, " 40"
'	Print #1, "0.18"
'	Print #1, "  9"
'	Print #1, "$DIMCEN"
'	Print #1, " 40"
'	Print #1, "0.09"
'	Print #1, "  9"
'	Print #1, "$DIMTSZ"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$DIMTOL"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMLIM"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMTIH"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DIMTOH"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DIMSE1"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMSE2"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMTAD"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMZIN"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMBLK"
'	Print #1, "  1"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$DIMASO"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DIMSHO"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DIMPOST"
'	Print #1, "  1"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$DIMAPOST"
'	Print #1, "  1"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$DIMALT"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMALTD"
'	Print #1, " 70"
'	Print #1, "     2"
'	Print #1, "  9"
'	Print #1, "$DIMALTF"
'	Print #1, " 40"
'	Print #1, "25.4"
'	Print #1, "  9"
'	Print #1, "$DIMLFAC"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$DIMTOFL"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMTVP"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$DIMTIX"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMSOXD"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMSAH"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMBLK1"
'	Print #1, "  1"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$DIMBLK2"
'	Print #1, "  1"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$DIMSTYLE"
'	Print #1, "  2"
'	Print #1, "STANDARD"
'	Print #1, "  9"
'	Print #1, "$DIMCLRD"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMCLRE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMCLRT"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMTFAC"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$DIMGAP"
'	Print #1, " 40"
'	Print #1, "0.09"
'	Print #1, "  9"
'	Print #1, "$DIMJUST"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMSD1"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMSD2"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMTOLJ"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$DIMTZIN"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMALTZ"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMALTTZ"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMFIT"
'	Print #1, " 70"
'	Print #1, "     3"
'	Print #1, "  9"
'	Print #1, "$DIMUPT"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$DIMUNIT"
'	Print #1, " 70"
'	Print #1, "     2"
'	Print #1, "  9"
'	Print #1, "$DIMDEC"
'	Print #1, " 70"
'	Print #1, "     4"
'	Print #1, "  9"
'	Print #1, "$DIMTDEC"
'	Print #1, " 70"
'	Print #1, "     4"
'	Print #1, "  9"
'	Print #1, "$DIMALTU"
'	Print #1, " 70"
'	Print #1, "     2"
'	Print #1, "  9"
'	Print #1, "$DIMALTTD"
'	Print #1, " 70"
'	Print #1, "     2"
'	Print #1, "  9"
'	Print #1, "$DIMTXSTY"
'	Print #1, "  7"
'	Print #1, "STANDARD"
'	Print #1, "  9"
'	Print #1, "$DIMAUNIT"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$LUNITS"
'	Print #1, " 70"
'	Print #1, "     2"
'	Print #1, "  9"
'	Print #1, "$LUPREC"
'	Print #1, " 70"
'	Print #1, "     4"
'	Print #1, "  9"
'	Print #1, "$SKETCHINC"
'	Print #1, " 40"
'	Print #1, "0.1"
'	Print #1, "  9"
'	Print #1, "$FILLETRAD"
'	Print #1, " 40"
'	Print #1, "0.5"
'	Print #1, "  9"
'	Print #1, "$AUNITS"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$AUPREC"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$MENU"
'	Print #1, "  1"
'	Print #1, "."
'	Print #1, "  9"
'	Print #1, "$ELEVATION"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PELEVATION"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$THICKNESS"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$LIMCHECK"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$BLIPMODE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$CHAMFERA"
'	Print #1, " 40"
'	Print #1, "0.5"
'	Print #1, "  9"
'	Print #1, "$CHAMFERB"
'	Print #1, " 40"
'	Print #1, "0.5"
'	Print #1, "  9"
'	Print #1, "$CHAMFERC"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$CHAMFERD"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$SKPOLY"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$TDCREATE"
'	Print #1, " 40"
'	Print #1, "2454799.543603726"
'	Print #1, "  9"
'	Print #1, "$TDUPDATE"
'	Print #1, " 40"
'	Print #1, "2454801.386249838"
'	Print #1, "  9"
'	Print #1, "$TDINDWG"
'	Print #1, " 40"
'	Print #1, "0.0303911806"
'	Print #1, "  9"
'	Print #1, "$TDUSRTIMER"
'	Print #1, " 40"
'	Print #1, "0.0303873611"
'	Print #1, "  9"
'	Print #1, "$USRTIMER"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$ANGBASE"
'	Print #1, " 50"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$ANGDIR"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$PDMODE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$PDSIZE"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PLINEWID"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$COORDS"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$SPLFRAME"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$SPLINETYPE"
'	Print #1, " 70"
'	Print #1, "     6"
'	Print #1, "  9"
'	Print #1, "$SPLINESEGS"
'	Print #1, " 70"
'	Print #1, "     8"
'	Print #1, "  9"
'	Print #1, "$ATTDIA"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$ATTREQ"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$HANDLING"
'	Print #1, " 70"
'	Print #1, "  4097"
End Sub
Sub dxfheader3()'inactive dxf header cont.
'	Print #1, "  9"
'	Print #1, "$SURFTAB1"
'	Print #1, " 70"
'	Print #1, "     6"
'	Print #1, "  9"
'	Print #1, "$SURFTAB2"
'	Print #1, " 70"
'	Print #1, "     6"
'	Print #1, "  9"
'	Print #1, "$SURFTYPE"
'	Print #1, " 70"
'	Print #1, "     6"
'	Print #1, "  9"
'	Print #1, "$SURFU"
'	Print #1, " 70"
'	Print #1, "     6"
'	Print #1, "  9"
'	Print #1, "$SURFV"
'	Print #1, " 70"
'	Print #1, "     6"
'	Print #1, "  9"
'	Print #1, "$UCSNAME"
'	Print #1, "  2"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$UCSORG"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$UCSXDIR"
'	Print #1, " 10"
'	Print #1, "1.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$UCSYDIR"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "1.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PUCSNAME"
'	Print #1, "  2"
'	Print #1, ""
'	Print #1, "  9"
'	Print #1, "$PUCSORG"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PUCSXDIR"
'	Print #1, " 10"
'	Print #1, "1.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PUCSYDIR"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "1.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$USERI1"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$USERI2"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$USERI3"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$USERI4"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$USERI5"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$USERR1"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$USERR2"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$USERR3"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$USERR4"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$USERR5"
'	Print #1, " 40"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$WORLDVIEW"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$SHADEDGE"
'	Print #1, " 70"
'	Print #1, "     3"
'	Print #1, "  9"
'	Print #1, "$SHADEDIF"
'	Print #1, " 70"
'	Print #1, "    70"
'	Print #1, "  9"
'	Print #1, "$TILEMODE"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$MAXACTVP"
'	Print #1, " 70"
'	Print #1, "    64"
'	Print #1, "  9"
'	Print #1, "$PINSBASE"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, " 30"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PLIMCHECK"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$PEXTMIN"
'	Print #1, " 10"
'	Print #1, "1.000000000000000E+20"
'	Print #1, " 20"
'	Print #1, "1.000000000000000E+20"
'	Print #1, " 30"
'	Print #1, "1.000000000000000E+20"
'	Print #1, "  9"
'	Print #1, "$PEXTMAX"
'	Print #1, " 10"
'	Print #1, "-1.000000000000000E+20"
'	Print #1, " 20"
'	Print #1, "-1.000000000000000E+20"
'	Print #1, " 30"
'	Print #1, "-1.000000000000000E+20"
'	Print #1, "  9"
'	Print #1, "$PLIMMIN"
'	Print #1, " 10"
'	Print #1, "0.0"
'	Print #1, " 20"
'	Print #1, "0.0"
'	Print #1, "  9"
'	Print #1, "$PLIMMAX"
'	Print #1, " 10"
'	Print #1, "12.0"
'	Print #1, " 20"
'	Print #1, "9.0"
'	Print #1, "  9"
'	Print #1, "$UNITMODE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$VISRETAIN"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$PLINEGEN"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$PSLTSCALE"
'	Print #1, " 70"
'	Print #1, "     1"
'	Print #1, "  9"
'	Print #1, "$TREEDEPTH"
'	Print #1, " 70"
'	Print #1, "  3020"
'	Print #1, "  9"
'	Print #1, "$PICKSTYLE"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$CMLSTYLE"
'	Print #1, "  2"
'	Print #1, "STANDARD"
'	Print #1, "  9"
'	Print #1, "$CMLJUST"
'	Print #1, " 70"
'	Print #1, "     0"
'	Print #1, "  9"
'	Print #1, "$CMLSCALE"
'	Print #1, " 40"
'	Print #1, "1.0"
'	Print #1, "  9"
'	Print #1, "$SAVEIMAGES"
'	Print #1, " 70"
'	Print #1, "     1"
End Sub
Sub dxfclasses()'inactive dxf classes
	'let's hold off on classes since it's all ac2k stuff
	Exit sub
	Print #1, "  0"
	Print #1, "SECTION"
	Print #1, "  2"
	Print #1, "CLASSES"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "ACDBDICTIONARYWDFLT"
	Print #1, "  2"
	Print #1, "AcDbDictionaryWithDefault"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "ACDBPLACEHOLDER"
	Print #1, "  2"
	Print #1, "AcDbPlaceHolder"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LAYOUT"
	Print #1, "  2"
	Print #1, "AcDbLayout"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LONG_TRANSACTION"
	Print #1, "  2"
	Print #1, "AcDbLongTransaction"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LWPOLYLINE"
	Print #1, "  2"
	Print #1, "AcDbPolyline"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "HATCH"
	Print #1, "  2"
	Print #1, "AcDbHatch"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "XRECORD"
	Print #1, "  2"
	Print #1, "AcDbXrecord"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LONG_TRANSACTION"
	Print #1, "  2"
	Print #1, "AcDbLongTransaction"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LWPOLYLINE"
	Print #1, "  2"
	Print #1, "AcDbPolyline"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "HATCH"
	Print #1, "  2"
	Print #1, "AcDbHatch"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "XRECORD"
	Print #1, "  2"
	Print #1, "AcDbXrecord"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LONG_TRANSACTION"
	Print #1, "  2"
	Print #1, "AcDbLongTransaction"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LWPOLYLINE"
	Print #1, "  2"
	Print #1, "AcDbPolyline"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "HATCH"
	Print #1, "  2"
	Print #1, "AcDbHatch"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "XRECORD"
	Print #1, "  2"
	Print #1, "AcDbXrecord"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LONG_TRANSACTION"
	Print #1, "  2"
	Print #1, "AcDbLongTransaction"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LWPOLYLINE"
	Print #1, "  2"
	Print #1, "AcDbPolyline"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "HATCH"
	Print #1, "  2"
	Print #1, "AcDbHatch"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "XRECORD"
	Print #1, "  2"
	Print #1, "AcDbXrecord"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LONG_TRANSACTION"
	Print #1, "  2"
	Print #1, "AcDbLongTransaction"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LWPOLYLINE"
	Print #1, "  2"
	Print #1, "AcDbPolyline"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "HATCH"
	Print #1, "  2"
	Print #1, "AcDbHatch"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "XRECORD"
	Print #1, "  2"
	Print #1, "AcDbXrecord"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LONG_TRANSACTION"
	Print #1, "  2"
	Print #1, "AcDbLongTransaction"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "LWPOLYLINE"
	Print #1, "  2"
	Print #1, "AcDbPolyline"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "HATCH"
	Print #1, "  2"
	Print #1, "AcDbHatch"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "CLASS"
	Print #1, "  1"
	Print #1, "XRECORD"
	Print #1, "  2"
	Print #1, "AcDbXrecord"
	Print #1, "  3"
	Print #1, "AutoCAD 2000"
	Print #1, " 90"
	Print #1, "    32768"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "ENDSEC"
	'end of classes
End Sub
Sub dxftablevport()'active with handles dxf tables - vport
	If savedxfblockonly=TRUE Then
		'saving block extents are done
	Else
		adjustextents
	EndIf
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "VPORT"
	Print #1, "  5"
	Print #1, "8"'this is pre handle - how do i handle pre handles
'	Print #1, "330"'all 330's are not required so i comment them out
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     2"
	Print #1, "  0"
	Print #1, "VPORT"
	Print #1, "  5"
	Print #1, Hex$(dxfhandlet-2)'"EC"'this is post handle entityt+1
'	Print #1, "330"
'	Print #1, "8"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbViewportTableRecord"
	Print #1, "  2"
	Print #1, "*ACTIVE"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 10"
	Print #1, "0.0"
	Print #1, " 20"
	Print #1, "0.0"
	Print #1, " 11"
	Print #1, "1.0"
	Print #1, " 21"
	Print #1, "1.0"
	Print #1, " 12"
	Print #1, LTrim(Str(extentsx1+(extentsx2-extentsx1)/2))
	Print #1, " 22"
	Print #1, LTrim(Str(extentsy1+(extentsy2-extentsy1)/2))
	Print #1, " 13"
	Print #1, "0.0"
	Print #1, " 23"
	Print #1, "0.0"
	Print #1, " 14"
	Print #1, "1.0"
	Print #1, " 24"
	Print #1, "1.0"
	Print #1, " 15"
	Print #1, "0.0"
	Print #1, " 25"
	Print #1, "0.0"
	Print #1, " 16"
	Print #1, "0.0"
	Print #1, " 26"
	Print #1, "0.0"
	Print #1, " 36"
	Print #1, "1.0"
	Print #1, " 17"
	Print #1, "0.0"
	Print #1, " 27"
	Print #1, "0.0"
	Print #1, " 37"
	Print #1, "0.0"
	Print #1, " 40"
	If extentsx2-extentsx1>=extentsy2-extentsy1 Then
		Print #1, LTrim(Str(extentsx2-extentsx1))
	Else
		Print #1, LTrim(Str(extentsy2-extentsy1))
	EndIf
	Print #1, " 41"
	Print #1, "2"
	Print #1, " 42"
	Print #1, "50.0"
	Print #1, " 43"
	Print #1, "0.0"
	Print #1, " 44"
	Print #1, "0.0"
	Print #1, " 50"
	Print #1, "0.0"
	Print #1, " 51"
	Print #1, "0.0"
	Print #1, " 71"
	Print #1, "     0"
	Print #1, " 72"
	Print #1, "   100"
	Print #1, " 73"
	Print #1, "     1"
	Print #1, " 74"
	Print #1, "     1"
	Print #1, " 75"
	Print #1, "     0"
	Print #1, " 76"
	Print #1, "     0"
	Print #1, " 77"
	Print #1, "     0"
	Print #1, " 78"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "ENDTAB"
	'there's a lot more to view port but pushing on...
End Sub
Sub dxftableltype()'active with handles dxf tables - LTYPE
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "LTYPE"
	Print #1, "  5"
	Print #1, "5"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "LTYPE"
	Print #1, "  5"
	Print #1, "14"
'	Print #1, "330"
'	Print #1, "5"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLinetypeTableRecord"
	Print #1, "  2"
	Print #1, "BYBLOCK"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  3"
	Print #1, ""
	Print #1, " 72"
	Print #1, "    65"
	Print #1, " 73"
	Print #1, "     0"
	Print #1, " 40"
	Print #1, "0.0"
	Print #1, "  0"
	Print #1, "LTYPE"
	Print #1, "  5"
	Print #1, "15"
'	Print #1, "330"
'	Print #1, "5"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLinetypeTableRecord"
	Print #1, "  2"
	Print #1, "BYLAYER"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  3"
	Print #1, ""
	Print #1, " 72"
	Print #1, "    65"
	Print #1, " 73"
	Print #1, "     0"
	Print #1, " 40"
	Print #1, "0.0"
	Print #1, "  0"
	Print #1, "LTYPE"
	Print #1, "  5"
	Print #1, "16"
'	Print #1, "330"
'	Print #1, "5"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLinetypeTableRecord"
	Print #1, "  2"
	Print #1, "CONTINUOUS"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  3"
	Print #1, "Solid line"
	Print #1, " 72"
	Print #1, "    65"
	Print #1, " 73"
	Print #1, "     0"
	Print #1, " 40"
	Print #1, "0.0"
	Print #1, "  0"
	Print #1, "ENDTAB"
End Sub
Sub dxftablelayers()'active with handles dxf tables - LAYERS need to mod this and mod handles if adding new layers
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "2"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "    16"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "10"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "0"'Layer Zero
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     7"'color #7
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "4E"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER1"'layer issue???
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     1"'color #???
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "4F"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER2"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     2"'color #???
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "50"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER3"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     3"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "51"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER4"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     4"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "52"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER5"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     5"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "53"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER6"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     6"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "54"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER7"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     7"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "55"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER8"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     8"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "56"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER9"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "     9"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "57"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER10"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "    10"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "58"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER11"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "    11"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "59"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER12"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "    12"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "5A"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER13"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "    13"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "5B"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER14"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "    14"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "LAYER"
	Print #1, "  5"
	Print #1, "5C"
'	Print #1, "330"
'	Print #1, "2"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbLayerTableRecord"
	Print #1, "  2"
	Print #1, "MYLAYER15"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 62"
	Print #1, "    15"
	Print #1, "  6"
	Print #1, "CONTINUOUS"
	Print #1, "  0"
	Print #1, "ENDTAB"
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "STYLE"
	Print #1, "  5"
	Print #1, "3"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "STYLE"
	Print #1, "  5"
	Print #1, "11"
'	Print #1, "330"
'	Print #1, "3"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbTextStyleTableRecord"
	Print #1, "  2"
	Print #1, "STANDARD"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 40"
	Print #1, "0.0"
	Print #1, " 41"
	Print #1, "1.0"
	Print #1, " 50"
	Print #1, "0.0"
	Print #1, " 71"
	Print #1, "     0"
	Print #1, " 42"
	Print #1, "0.2"
	Print #1, "  3"
	Print #1, "txt"
	Print #1, "  4"
	Print #1, ""
	Print #1, "  0"
	Print #1, "ENDTAB"
End Sub
Sub dxftableviewucsappiddimstyle()'active with handles dxf tables - VIEW, UCS, APPID, and DIMSTYLE
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "VIEW"
	Print #1, "  5"
	Print #1, "6"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "ENDTAB"
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "UCS"
	Print #1, "  5"
	Print #1, "7"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "ENDTAB"
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "APPID"
	Print #1, "  5"
	Print #1, "9"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "APPID"
	Print #1, "  5"
	Print #1, "12"
'	Print #1, "330"
'	Print #1, "9"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbRegAppTableRecord"
	Print #1, "  2"
	Print #1, "ACAD"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "ENDTAB"
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "DIMSTYLE"
	Print #1, "  5"
	Print #1, "A"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "DIMSTYLE"
	Print #1, "105"
	Print #1, "27"
'	Print #1, "330"
'	Print #1, "A"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbDimStyleTableRecord"
	Print #1, "  2"
	Print #1, "STANDARD"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  3"
	Print #1, ""
	Print #1, "  4"
	Print #1, ""
	Print #1, "  5"
	Print #1, ""
	Print #1, "  6"
	Print #1, ""
	Print #1, "  7"
	Print #1, ""
	Print #1, " 40"
	Print #1, "1.0"
	Print #1, " 41"
	Print #1, "0.18"
	Print #1, " 42"
	Print #1, "0.0625"
	Print #1, " 43"
	Print #1, "0.38"
	Print #1, " 44"
	Print #1, "0.18"
	Print #1, " 45"
	Print #1, "0.0"
	Print #1, " 46"
	Print #1, "0.0"
	Print #1, " 47"
	Print #1, "0.0"
	Print #1, " 48"
	Print #1, "0.0"
	Print #1, "140"
	Print #1, "0.18"
	Print #1, "141"
	Print #1, "0.09"
	Print #1, "142"
	Print #1, "0.0"
	Print #1, "143"
	Print #1, "25.4"
	Print #1, "144"
	Print #1, "1.0"
	Print #1, "145"
	Print #1, "0.0"
	Print #1, "146"
	Print #1, "1.0"
	Print #1, "147"
	Print #1, "0.09"
	Print #1, " 71"
	Print #1, "     0"
	Print #1, " 72"
	Print #1, "     0"
	Print #1, " 73"
	Print #1, "     1"
	Print #1, " 74"
	Print #1, "     1"
	Print #1, " 75"
	Print #1, "     0"
	Print #1, " 76"
	Print #1, "     0"
	Print #1, " 77"
	Print #1, "     0"
	Print #1, " 78"
	Print #1, "     0"
	Print #1, "170"
	Print #1, "     0"
	Print #1, "171"
	Print #1, "     2"
	Print #1, "172"
	Print #1, "     0"
	Print #1, "173"
	Print #1, "     0"
	Print #1, "174"
	Print #1, "     0"
	Print #1, "175"
	Print #1, "     0"
	Print #1, "176"
	Print #1, "     0"
	Print #1, "177"
	Print #1, "     0"
	Print #1, "178"
	Print #1, "     0"
	Print #1, "270"
	Print #1, "     2"
	Print #1, "271"
	Print #1, "     4"
	Print #1, "272"
	Print #1, "     4"
	Print #1, "273"
	Print #1, "     2"
	Print #1, "274"
	Print #1, "     2"
	Print #1, "340"
	Print #1, "11"
	Print #1, "275"
	Print #1, "     0"
	Print #1, "280"
	Print #1, "     0"
	Print #1, "281"
	Print #1, "     0"
	Print #1, "282"
	Print #1, "     0"
	Print #1, "283"
	Print #1, "     1"
	Print #1, "284"
	Print #1, "     0"
	Print #1, "285"
	Print #1, "     0"
	Print #1, "286"
	Print #1, "     0"
	Print #1, "287"
	Print #1, "     3"
	Print #1, "288"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "ENDTAB"
End Sub
Sub dxftableblocks()'active with handles dxf tables - block records
	'calls sub routine to build blocks data at the bottom of this
	Dim As Integer i,j,k,l
	Print #1, "  0"
	Print #1, "TABLE"
	Print #1, "  2"
	Print #1, "BLOCK_RECORD"
	Print #1, "  5"
	Print #1, "1"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbSymbolTable"
	Print #1, " 70"
	Print #1, "     1"
	Print #1, "  0"
	Print #1, "BLOCK_RECORD"
	Print #1, "  5"
	Print #1, "1F"'this is a pre handle
	Print #1, "102"
	Print #1, "{ACAD_XDICTIONARY"
	Print #1, "360"'do i need 360 handles - 330's are out maybe 360's too - did quick test - 360's are not needed
	Print #1, "65"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "1"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbBlockTableRecord"
	Print #1, "  2"
	Print #1, "*MODEL_SPACE"
	Print #1, "  0"
	Print #1, "BLOCK_RECORD"
	Print #1, "  5"
	Print #1, "1B"'this is a pre handle
	Print #1, "102"
	Print #1, "{ACAD_XDICTIONARY"
	Print #1, "360"
	Print #1, "63"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "1"
	Print #1, "100"
	Print #1, "AcDbSymbolTableRecord"
	Print #1, "100"
	Print #1, "AcDbBlockTableRecord"
	Print #1, "  2"
	Print #1, "*PAPER_SPACE"
	
	If savedxfblockonly=TRUE Then
		Open "fbcadtempdxfentitydata.dat" For Output As #3
		Close #3
		Open "fbcadtempdxfentitydata.dat" For Append As #3
	Else 
		'for each block do this in the table section ***
		'are there any blocks to create???
		'and are those blocks deleted or active???
	
		'opening and closing this file creates it / clears it for use
		Open "fbcadtempdxfblockdata.dat" For Output As #2
		Close #2
		Open "fbcadtempdxfblockdata.dat" For Append As #2'why append? could simple be output
		
		Open "fbcadtempdxfentitydata.dat" For Output As #3
		Close #3
		Open "fbcadtempdxfentitydata.dat" For Append As #3'why append? could simple be output
		
		
		If createdxfblockdata=TRUE Then
	
			'hey wait all this shit must be know in order to set HANDSEED in the header
			'i need to know how many blocks and how many entities in each block
			'are to be included in the dxf file prior to coming here.
	'		Do this For each active block (Not DUPLICATE BLOCKS)
	'		Note:every block (in the drawing dupes included) via INSERT is in the entities section
	
	
			'not sure why things like dupeblos() is not dim shared
			'cuz wasn't all this figured out previous to calling this sub routine
			'oh, perhaps that's why i was wondering about handseed count in note above
			'anyhow, it's ony an opportunity to speed up saving proces
			Dim dupeblocks(blockc) As String
			Dim dupeblocksc As Integer
			Dim As BOOLEAN activeblock, dupeblock
			dupeblocksc=0
			For j=1 To blockc
				activeblock=FALSE
				For i = 1 To linec
					if lines(i,8)>=0 And lines(i,9)=j Then
						dupeblock=FALSE
						For k=1 To dupeblocksc
							If dupeblocks(k)=blocknames(j) Then
								dupeblock=TRUE
								Exit for
							EndIf
						Next
						If dupeblock=FALSE Then
							activeblock=TRUE
	'						entityt=entityt+1
							'this is not a dupe block
						End If
					End If
				Next
				For i = 1 To circlec
					if circles(i,10)>=0 And circles(i,12)=j Then
						dupeblock=FALSE
						For k=1 To dupeblocksc
							If dupeblocks(k)=blocknames(j) Then
								dupeblock=TRUE
								Exit for
							EndIf
						Next
						If dupeblock=FALSE Then
							activeblock=TRUE
	'						entityt=entityt+1
						End If
					End If
				Next
				'each active block uses 3 handles
				'D7 is in the tables section
				'D8 and D9 are in the block section
				If activeblock=TRUE Then
					dupeblocksc=dupeblocksc+1
					dupeblocks(dupeblocksc)=blocknames(j)
					'because there is an active block
	'				entityt=entityt+1'for the insert of the block in the entities section
	'				If dupeblock=FALSE Then
	'					entityt=entityt+3'for ? in ? section (tables???)
	'				End If
					Print #1, "  0"
					Print #1, "BLOCK_RECORD"
					Print #1, "  5"'remember this is just the table section
					dxfhandlec=dxfhandlec+1
					Print #1, Hex$(dxfhandlec)'"D7"'D7,8 & 9 are post handles - looks like each block needs 3 handlse allocated in addition to it's entities and the handles for each entity
				'	Print #1, "330"
				'	Print #1, "1"
					Print #1, "100"
					Print #1, "AcDbSymbolTableRecord"
					Print #1, "100"
					Print #1, "AcDbBlockTableRecord"
					Print #1, "  2"
					Print #1, blocknames(j)'"TEST_BLOCK1"
					'now build inties for this block and incriment handle counter
					'by calling the sub for entites which is
					dxfbuildblocks(j) 'but it can't write to this file yet
					'only top level blocks are inserted here
					If blocklevelc>0 Then
						For l=1 To blockc
							'this is my first change to dxf info in a few years
							'this is in the dxftableblocks sub
							If blockstatus(l)=0 And blocknames(l)=blocknames(j)And blocklevel(l,1)=0 Then
								'now open for append the entities file
								'and ad the INSERT info for this block based on how many times
								'the block is in the drawing
								'this is where the blockoffset info is used
									'insertion of blocks here in the entity section
								'using the dxf entity type "INSERT"
								'presents a LAYER issue... what if there are lines and circles
								'in the block that are of various layers???
								Print #3, "  0"
								Print #3, "INSERT"
								Print #3, "  5"
								dxfhandlec=dxfhandlec+1
								Print #3, Hex$(dxfhandlec)'"EB"
							'	Print #2, "330"
							'	Print #2, "1F"
								Print #3, "100"
								Print #3, "AcDbEntity"
								Print #3, "  8"
								Print #3, "MYLAYER15"'this is the layer issue i see
								Print #3, "100"
								Print #3, "AcDbBlockReference"
								Print #3, "  2"
								Print #3, blocknames(l)'"TEST_BLOCK1"
							'this is the insertion point ... my offsetxyz points
								print #3, " 10"
								tempstring=str(blockoffsets(l,1))
								csntodec
								print #3, tempstring
								print #3, " 20"
								tempstring=str(blockoffsets(l,2))
								csntodec
								print #3, tempstring
								print #3, " 30"
								tempstring=str(blockoffsets(l,3))
								csntodec
								print #3, tempstring
								print #3, " 41"
								tempstring=str(blockoffsets(l,4))
								csntodec
								print #3, tempstring
								print #3, " 42"
								tempstring=str(blockoffsets(l,5))
								csntodec
								print #3, tempstring
								print #3, " 43"
								tempstring=str(blockoffsets(l,6))
								csntodec
								print #3, tempstring
								print #3, " 50"
								tempstring=str(blockoffsets(l,7))
								csntodec
								print #3, tempstring
							End If
						next
					Else
						For l=1 To blockc
							'this is my first change to dxf info in a few years
							'this is in the dxftableblocks sub
							If blockstatus(l)=0 And blocknames(l)=blocknames(j) Then
								'now open for append the entities file
								'and ad the INSERT info for this block based on how many times
								'the block is in the drawing
								'this is where the blockoffset info is used
									'insertion of blocks here in the entity section
								'using the dxf entity type "INSERT"
								'presents a LAYER issue... what if there are lines and circles
								'in the block that are of various layers???
								Print #3, "  0"
								Print #3, "INSERT"
								Print #3, "  5"
								dxfhandlec=dxfhandlec+1
								Print #3, Hex$(dxfhandlec)'"EB"
							'	Print #2, "330"
							'	Print #2, "1F"
								Print #3, "100"
								Print #3, "AcDbEntity"
								Print #3, "  8"
								Print #3, "MYLAYER15"'this is the layer issue i see
								Print #3, "100"
								Print #3, "AcDbBlockReference"
								Print #3, "  2"
								Print #3, blocknames(l)'"TEST_BLOCK1"
							'this is the insertion point ... my offsetxyz points
								print #3, " 10"
								tempstring=str(blockoffsets(l,1))
								csntodec
								print #3, tempstring
								print #3, " 20"
								tempstring=str(blockoffsets(l,2))
								csntodec
								print #3, tempstring
								print #3, " 30"
								tempstring=str(blockoffsets(l,3))
								csntodec
								print #3, tempstring
								print #3, " 41"
								tempstring=str(blockoffsets(l,4))
								csntodec
								print #3, tempstring
								print #3, " 42"
								tempstring=str(blockoffsets(l,5))
								csntodec
								print #3, tempstring
								print #3, " 43"
								tempstring=str(blockoffsets(l,6))
								csntodec
								print #3, tempstring
								print #3, " 50"
								tempstring=str(blockoffsets(l,7))
								csntodec
								print #3, tempstring
							End If
						next
					EndIf
				EndIf
			Next
		End If
	End If
	Print #1, "  0"
	Print #1, "ENDTAB"
End Sub
Sub dxfblockpreamble()'dxf block section preamble
	'after this the file created from dxfbuildblocks are added
	'and does this depend on if there are any blocks at all to write???
	'yes this section gets written even if there are no blocks
	'it's the next part after this PREAMBLE that may not get created
	'depending on wether or not there are any blocks to create 
	Print #1, "  0"
	Print #1, "SECTION"
	Print #1, "  2"
	Print #1, "BLOCKS"
	Print #1, "  0"
	Print #1, "BLOCK"
	Print #1, "  5"
	Print #1, "20"'this is pre handle
'	Print #1, "330"
'	Print #1, "1F"
	Print #1, "100"
	Print #1, "AcDbEntity"
	Print #1, "  8"
	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbBlockBegin"
	Print #1, "  2"
	Print #1, "*MODEL_SPACE"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 10"
	Print #1, "0.0"
	Print #1, " 20"
	Print #1, "0.0"
	Print #1, " 30"
	Print #1, "0.0"
	Print #1, "  3"
	Print #1, "*MODEL_SPACE"
	Print #1, "  1"
	Print #1, ""
	Print #1, "  0"
	Print #1, "ENDBLK"
	Print #1, "  5"
	Print #1, "21"'this is pre handle
'	Print #1, "330"
'	Print #1, "1F"
	Print #1, "100"
	Print #1, "AcDbEntity"
	Print #1, "  8"
	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbBlockEnd"
	Print #1, "  0"
	Print #1, "BLOCK"
	Print #1, "  5"
	Print #1, "1C"'this is pre handle
'	Print #1, "330"
'	Print #1, "1B"
	Print #1, "100"
	Print #1, "AcDbEntity"
	Print #1, " 67"
	Print #1, "     1"
	Print #1, "  8"
	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbBlockBegin"
	Print #1, "  2"
	Print #1, "*PAPER_SPACE"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, " 10"
	Print #1, "0.0"
	Print #1, " 20"
	Print #1, "0.0"
	Print #1, " 30"
	Print #1, "0.0"
	Print #1, "  3"
	Print #1, "*PAPER_SPACE"
	Print #1, "  1"
	Print #1, ""
	Print #1, "  0"
	Print #1, "ENDBLK"
	Print #1, "  5"
	Print #1, "1D"'this is pre handle
'	Print #1, "330"
'	Print #1, "1B"
	Print #1, "100"
	Print #1, "AcDbEntity"
	Print #1, " 67"
	Print #1, "     1"
	Print #1, "  8"
	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbBlockEnd"
End Sub
Sub dxfbuildblocks(dxfblocki As Integer)'dxf builds file for each block it's entities
	'block enties handls are incrimented and writen to file #2
	'open #2 for append
	

	'so i got to have a dxfblocki counter going previously
	'in order to itterate thru the blocks
	'and durring that itteration i need to make sure that
	'the block is not deleted
	
	'pending - it is in this sub where i deal with nested blocks
	'or is in when in this sub it calls dxfblockentities
	Dim As Integer i
	Print #2, "  0"
	Print #2, "BLOCK"
	Print #2, "  5"
	dxfhandlec=dxfhandlec+1
	Print #2, Hex$(dxfhandlec)'"D8"'this is post handle
	dxfhandlec=dxfhandlec+1
	Dim temphandle As Integer
	temphandle=dxfhandlec
'	Print #1, "330"
'	Print #1, "D7"
	Print #2, "100"
	Print #2, "AcDbEntity"
	Print #2, "  8"
	Print #2, "0"
	Print #2, "100"
	Print #2, "AcDbBlockBegin"
	Print #2, "  2"
	Print #2, blocknames(dxfblocki)'"TEST_BLOCK1"
	Print #2, " 70"
	Print #2, "     0"
	'no the confusion is a bit clearer
	'this insertion info must be for nested blocks
	'(maybe) look below an notice blockname is this blockname not nested block name 
	Print #2, " 10"'is this the insertion point???
	Print #2, "0.0"
	Print #2, " 20"
	Print #2, "0.0"
	Print #2, " 30"
	Print #2, "0.0"
	Print #2, "  3"
	Print #2, blocknames(dxfblocki)'"TEST_BLOCK1"'what is the name of the block(dxfblocki)
	Print #2, "  1"
	Print #2, ""
	'in each block do this for all it's entities (DA thru EA was in example)
	'print #2 not #1
		'this is where i got to move the block to 0,0
	'and move it back after done
'	modifyx1=blockoffsets(dxfblocki,1)
'	modifyy1=blockoffsets(dxfblocki,2)
'	modifyx2=0
'	modifyy2=0
'	movedxfblock(dxfblocki)
	For i = 1 To blockc 'why am i looking for first occurance of block
		'it's suposed to be referencing the master block (first must be master)
		If blocknames(i)=blocknames(dxfblocki) Then
			dxfblockentities(i)
			Exit for
		EndIf
	next
	
	
	
'	modifyx1=0
'	modifyy1=0
'	modifyx2=blockoffsets(dxfblocki,1)
'	modifyy2=blockoffsets(dxfblocki,2)
'	movedxfblock(dxfblocki)

	'write the entites in the block
	'done doing each entity for each block
	'********* where is D8
	'now close out the block
	Print #2, "  0"
	Print #2, "ENDBLK"
	Print #2, "  5"
	'here's where to put temphandle
	Print #2, Hex$(temphandle)'"D9"'this is post handle
'	Print #2, "330"
'	Print #2, "D7"
'is this stuff below part of each block???
'or is it the last part of the block section
'remember if this is part of each blk then
	'print #2 not #1
	Print #2, "100"
	Print #2, "AcDbEntity"
	Print #2, "  8"
	Print #2, "0"
	Print #2, "100"
	Print #2, "AcDbBlockEnd"
End Sub
Sub dxfblockentities(dxfblocknumber As Integer)'dxf block entities section
	'i got to do this with a particular block in mind
	'the if line(i,8)... ,8 varialbe is a variable for any
	'entity >=0 is (not deleted)
	'what variable did i use for active blocks
	'lines(i,9)=blocknumber
	'this is where i got to move the block to 0,0
	'and move it back after done
	
	Dim As Integer i,j,k,l
	for i = 1 to linec
		if lines(i,9)=dxfblocknumber then
			print #2, "  0"
			print #2, "LINE"
			print #2, "  5"
			dxfhandlec=dxfhandlec+1
			print #2, Hex$(dxfhandlec)
			print #2, "100"
			print #2, "AcDbEntity"
			print #2, "  8"
			print #2, "MYLAYER"+ltrim(str(lines(i,7)))
			print #2, "100"
			print #2, "AcDbLine"
			print #2, " 10"
			tempstring=str(lines(i,1))
			csntodec
			print #2, tempstring
			print #2, " 20"
			tempstring=str(lines(i,2))
			csntodec
			print #2, tempstring
			print #2, " 30"
			tempstring=str(lines(i,3))
			csntodec
			print #2, tempstring
			print #2, " 11"
			tempstring=str(lines(i,4))
			csntodec
			print #2, tempstring
'			print #2, ltrim(str(lines(i,4)))
			print #2, " 21"
			tempstring=str(lines(i,5))
			csntodec
			print #2, tempstring
'			print #2, ltrim(str(lines(i,5)))
			print #2, " 31"
			tempstring=str(lines(i,6))
			csntodec
			print #2, tempstring
'			print #2, ltrim(str(lines(i,6)))
		End If
	Next
	'and what is the variable for circles that are blocks
	'circles(i,12)=blocknumber
	For i=1 To circlec
		If circles(i,12)=dxfblocknumber then
			Select Case circles(i,9)
				Case 1
					print #2, "  0"
					print #2, "CIRCLE"
					print #2, "  5"
					dxfhandlec=dxfhandlec+1
					print #2, Hex$(dxfhandlec)
					print #2, "100"
					print #2, "AcDbEntity"
					print #2, "  8"
					print #2, "MYLAYER"+ltrim(str(circles(i,5)))
					print #2, "100"
					print #2, "AcDbCircle"
					print #2, " 10"
					tempstring=str(circles(i,1))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,1)))
					print #2, " 20"
					tempstring=str(circles(i,2))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,2)))
					print #2, " 30"
					tempstring=str(circles(i,3))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,3)))
					print #2, " 40"
					tempstring=str(circles(i,4))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,4)))
				Case 2
					print #2, "  0"
					print #2, "ARC"
					print #2, "  5"
					dxfhandlec=dxfhandlec+1
					print #2, Hex$(dxfhandlec)
					print #2, "100"
					print #2, "AcDbEntity"
					print #2, "  8"
					print #2, "MYLAYER"+ltrim(str(circles(i,5)))
					print #2, "100"
					print #2, "AcDbCircle"
					print #2, " 10"
					tempstring=str(circles(i,1))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,1)))
					print #2, " 20"
					tempstring=str(circles(i,2))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,2)))
					print #2, " 30"
					tempstring=str(circles(i,3))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,3)))
					print #2, " 40"
					tempstring=str(circles(i,4))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,4)))
					print #2, "100"
					print #2, "AcDbArc"
					print #2, " 50"
					tempstring=str(circles(i,6)*180/PI)
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,6)*180/PI))
					print #2, " 51"
					tempstring=str(circles(i,7)*180/PI)
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,7)*180/PI))
				Case 3,4
					print #2, "  0"
					print #2, "ELLIPSE"
					print #2, "  5"
					dxfhandlec=dxfhandlec+1
					print #2, Hex$(dxfhandlec)
					print #2, "100"
					print #2, "AcDbEntity"
					print #2, "  8"
					print #2, "MYLAYER"+ltrim(str(circles(i,5)))
					print #2, "100"
					print #2, "AcDbEllipse"
					print #2, " 10"
					tempstring=str(circles(i,1))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,1)))
					print #2, " 20"
					tempstring=str(circles(i,2))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,2)))
					print #2, " 30"
					tempstring=str(circles(i,3))
					csntodec
					print #2, tempstring
					'print #2, ltrim(str(circles(i,3)))
					print #2, " 11"
					fx=cos((circles(i,11))*d2r)*circles(i,4)
					fy=sin((circles(i,11))*d2r)*circles(i,4)
					tempstring=str(fx)
					csntodec
					print #2, tempstring
					print #2, " 21"
					tempstring=str(fy)
					csntodec
					print #2, tempstring
					print #2, " 31"
					print #2, "0"
					print #2, "210"'not sure about these 200 series yet
					print #2, "0.0"
					print #2, "220"
					print #2, "0.0"
					print #2, "230"
					print #2, "1.0"'not sure what value this should be 0, 1 or -1
					print #2, " 40"
					tempstring=str(circles(i,8)/circles(i,4))
					csntodec
					print #2, tempstring
					print #2, " 41"
					tempstring=str(circles(i,6)*d2r)
					csntodec
					print #2, tempstring
					print #2, " 42"
					tempstring=str(circles(i,7)*d2r)
					csntodec
					print #2, tempstring
			End Select
		End If
	Next
	'now for nested block info
	'if this is a parent block it's children will reference it
	'ie.
	'1 master 2 copy of 1 (child to 6)
	'3 master 4 copy of 3 (child to 6)
	'5 master 6 copy of 5 (parent to 2 & 4)
	'keep in mind offsets are relative
	'and childs actual block number is it's master
	'***************
	'There is a known problem with nexted blocks in dxf
	'***************
		'instead of inserting the nested block - just add the entity info
		'of the lines and circles of that nested block
		'into the parent block
	'***************
	If blocklevelc>0 Then
		For l=1 To blockc
			'this is my first change to dxf info in a few years
			'If blockstatus(l)<>TRUE And blocknames(l)=blocknames(j) Then
			If blockstatus(l)=0 And blocknames(l)=blocknames(dxfblocknumber) Then
				'if other blocks have block(l) as a parent
				'l=6
				'in general l should = dxfblocknumber + 1 but due the ability to
				'deletet this copy, and intead a second or subsequent copy of block
				'was used to instatiate this master bock definition, i must itterate
				'thru the list of blocks to find its first occurance. once first
				'occurance is found then exit for after copleting the followning:
				For i=1 To blockc
					'for every block whose parent is = to 6 i need to instert it's master block number
					'If blockstatus(i)=FALSE And blocklevel(i,blocklevel(i,1)+1)=l Then
					If blockstatus(i)=0 And blocklevel(i,2)=l Then
						'2 has 6 as parent
						'insert master of 2 which is 1
						For j=1 To blockc
							If blockstatus(j)=-1 And blocknames(j)=blocknames(i) Then
								Print #2, "  0"
								Print #2, "INSERT"
								Print #2, "  5"
								dxfhandlec=dxfhandlec+1
								Print #2, Hex$(dxfhandlec)'"EB"
								Print #2, "100"
								Print #2, "AcDbEntity"
								Print #2, "  8"
								Print #2, "MYLAYER15"'this is the layer issue i see
								Print #2, "100"
								Print #2, "AcDbBlockReference"
								Print #2, "  2"
								Print #2, blocknames(j)'"TEST_BLOCK1"
							'this is the insertion point ... my offsetxyz points
							'this needs to be relative to parent block
							'the nest children's insertion point needs to be
							'UNROTATED (TRANSFORMATION) - ROTATED OPPOSITE ITS PARENT
							'AND WHAT ABOUT SCALE???
'declare Sub rotatepoint(rptx As Double,rpty As Double,rpivotx As Double,rpivoty As Double,rangle As Double)
'	x1p=rptx-rpivotx
'	y1p=rpty-rpivoty
'	rotatedptx=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
'	rotatedpty=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
'End Sub
								rotatepoint(blockoffsets(i,1),blockoffsets(i,2),blockoffsets(l,1),blockoffsets(l,2),blockoffsets(l,7)*-1)
								'now scale factors
								rotatedptx=rotatedptx-blockoffsets(l,1)
								rotatedpty=rotatedpty-blockoffsets(l,2)
								'rotatedptz=rotatedptz-blockoffsets(l,3)
								If blockoffsets(l,4)<>0 And rotatedptx<>0 Then rotatedptx=rotatedptx/blockoffsets(l,4)
								If blockoffsets(l,5)<>0 And rotatedpty<>0 Then rotatedpty=rotatedpty/blockoffsets(l,5)
								
								Print #2, " 10"
								'tempstring=str(blockoffsets(i,1))
								'tempstring=str(blockoffsets(i,1)-blockoffsets(l,1))
								'tempstring=str(rotatedptx-blockoffsets(l,1))
								tempstring=str(rotatedptx)
								csntodec
								Print #2, tempstring
								Print #2, " 20"
								'tempstring=str(blockoffsets(i,2))
								'tempstring=str(blockoffsets(i,2)-blockoffsets(l,2))
								'tempstring=str(rotatedpty-blockoffsets(l,2))
								tempstring=str(rotatedpty)
								csntodec
								Print #2, tempstring
								Print #2, " 30"
								'tempstring=str(blockoffsets(i,3))
								tempstring=str(blockoffsets(i,3)-blockoffsets(l,3))
								csntodec
								Print #2, tempstring
								Print #2, " 41"
								'tempstring=str(blockoffsets(i,4))
								tempstring=str(blockoffsets(i,4)-blockoffsets(l,4))
								csntodec
								Print #2, tempstring
								Print #2, " 42"
								'tempstring=str(blockoffsets(i,5))
								tempstring=str(blockoffsets(i,5)-blockoffsets(l,5))
								csntodec
								Print #2, tempstring
								Print #2, " 43"
								tempstring=str(blockoffsets(i,6))
								csntodec
								Print #2, tempstring
								Print #2, " 50"
								tempstring=str(blockoffsets(i,7)-blockoffsets(l,7))
								csntodec
								Print #2, tempstring
								theboxbelow("nested block "+blocknames(j)+" inserted in "+ blocknames(dxfblocknumber) +" dxf file")
								Exit For
							EndIf
						Next
					EndIf
					
				Next
				Exit For
			End If
		Next
	End If
				
End Sub
Sub dxfentities()
	Dim As Integer i,datac,datac1,datac2,datac3
	If savedxfblockonly=TRUE Then
		for i = 1 to linec
			if lines(i,8)>=0 And lines(i,9)=savedxfblockonlyi then
				print #3, "  0"
				print #3, "LINE"
				print #3, "  5"
				dxfhandlec=dxfhandlec+1
				print #3, Hex$(dxfhandlec)
				print #3, "100"
				print #3, "AcDbEntity"
				print #3, "  8"
				print #3, "MYLAYER"+ltrim(str(lines(i,7)))
				print #3, "100"
				print #3, "AcDbLine"
				print #3, " 10"
				tempstring=str(lines(i,1))
				csntodec
				print #3, tempstring
				print #3, " 20"
				tempstring=str(lines(i,2))
				csntodec
				print #3, tempstring
				print #3, " 30"
				tempstring=str(lines(i,3))
				csntodec
				print #3, tempstring
				print #3, " 11"
				tempstring=str(lines(i,4))
				csntodec
				print #3, tempstring
	'			print #3, ltrim(str(lines(i,4)))
				print #3, " 21"
				tempstring=str(lines(i,5))
				csntodec
				print #3, tempstring
	'			print #3, ltrim(str(lines(i,5)))
				print #3, " 31"
				tempstring=str(lines(i,6))
				csntodec
				print #3, tempstring
	'			print #3, ltrim(str(lines(i,6)))
			End If
		Next
		'and what is the variable for circles that are blocks
		'circles(i,12)=blocknumber
		For i=1 To circlec
			If circles(i,10)>=0 And circles(i,12)=savedxfblockonlyi then
				Select Case circles(i,9)
					Case 1
						print #3, "  0"
						print #3, "CIRCLE"
						print #3, "  5"
						dxfhandlec=dxfhandlec+1
						print #3, Hex$(dxfhandlec)
						print #3, "100"
						print #3, "AcDbEntity"
						print #3, "  8"
						print #3, "MYLAYER"+ltrim(str(circles(i,5)))
						print #3, "100"
						print #3, "AcDbCircle"
						print #3, " 10"
						tempstring=str(circles(i,1))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,1)))
						print #3, " 20"
						tempstring=str(circles(i,2))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,2)))
						print #3, " 30"
						tempstring=str(circles(i,3))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,3)))
						print #3, " 40"
						tempstring=str(circles(i,4))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,4)))
					Case 2
						print #3, "  0"
						print #3, "ARC"
						print #3, "  5"
						dxfhandlec=dxfhandlec+1
						print #3, Hex$(dxfhandlec)
						print #3, "100"
						print #3, "AcDbEntity"
						print #3, "  8"
						print #3, "MYLAYER"+ltrim(str(circles(i,5)))
						print #3, "100"
						print #3, "AcDbCircle"
						print #3, " 10"
						tempstring=str(circles(i,1))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,1)))
						print #3, " 20"
						tempstring=str(circles(i,2))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,2)))
						print #3, " 30"
						tempstring=str(circles(i,3))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,3)))
						print #3, " 40"
						tempstring=str(circles(i,4))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,4)))
						print #3, "100"
						print #3, "AcDbArc"
						print #3, " 50"
						tempstring=str(circles(i,6)*180/PI)
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,6)*180/PI))
						print #3, " 51"
						tempstring=str(circles(i,7)*180/PI)
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,7)*180/PI))
					Case 3,4
						print #3, "  0"
						print #3, "ELLIPSE"
						print #3, "  5"
						dxfhandlec=dxfhandlec+1
						print #3, Hex$(dxfhandlec)
						print #3, "100"
						print #3, "AcDbEntity"
						print #3, "  8"
						print #3, "MYLAYER"+ltrim(str(circles(i,5)))
						print #3, "100"
						print #3, "AcDbEllipse"
						print #3, " 10"
						tempstring=str(circles(i,1))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,1)))
						print #3, " 20"
						tempstring=str(circles(i,2))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,2)))
						print #3, " 30"
						tempstring=str(circles(i,3))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,3)))
						print #3, " 11"
						fx=cos((circles(i,11))*d2r)*circles(i,4)
						fy=sin((circles(i,11))*d2r)*circles(i,4)
						tempstring=str(fx)
						csntodec
						print #3, tempstring
						print #3, " 21"
						tempstring=str(fy)
						csntodec
						print #3, tempstring
						print #3, " 31"
						print #3, "0"
						print #3, "210"'not sure about these 200 series yet
						print #3, "0.0"
						print #3, "220"
						print #3, "0.0"
						print #3, "230"
						print #3, "1.0"'not sure what value this should be 0, 1 or -1
						print #3, " 40"
						tempstring=str(circles(i,8)/circles(i,4))
						csntodec
						print #3, tempstring
						print #3, " 41"
						tempstring=str(circles(i,6)*d2r)
						csntodec
						print #3, tempstring
						print #3, " 42"
						tempstring=str(circles(i,7)*d2r)
						csntodec
						print #3, tempstring
				End Select
			End If
		Next
	Else
		Open "entities.dat" For Output As #6
		Open "entities-mod.dat" For Output As #7
		Open "entities-data.dat" For Output As #8
		Print #8, "DATA ";Chr(34);"ENTITY DATA START";Chr(34)
		Print #8, "DATA 0"
		datac=0
		for i = 1 to linec
			if lines(i,8)>=0 And lines(i,9)=0 then
				print #6, "LINE (";lines(i,1);",";lines(i,2);")-(";lines(i,4);",";lines(i,5);")"
				print #7, "LINE (";lines(i,1);" + x1,";lines(i,2);" + y1)-(";lines(i,4);" + x2,";lines(i,5);" + y2)"
				datac+=1
				print #3, "  0"
				print #3, "LINE"
				print #3, "  5"
				dxfhandlec=dxfhandlec+1
				print #3, Hex$(dxfhandlec)
				print #3, "100"
				print #3, "AcDbEntity"
				print #3, "  8"
				print #3, "MYLAYER"+ltrim(str(lines(i,7)))
				print #3, "100"
				print #3, "AcDbLine"
				print #3, " 10"
				tempstring=str(lines(i,1))
				csntodec
				print #3, tempstring
				print #3, " 20"
				tempstring=str(lines(i,2))
				csntodec
				print #3, tempstring
				print #3, " 30"
				tempstring=str(lines(i,3))
				csntodec
				print #3, tempstring
				print #3, " 11"
				tempstring=str(lines(i,4))
				csntodec
				print #3, tempstring
	'			print #3, ltrim(str(lines(i,4)))
				print #3, " 21"
				tempstring=str(lines(i,5))
				csntodec
				print #3, tempstring
	'			print #3, ltrim(str(lines(i,5)))
				print #3, " 31"
				tempstring=str(lines(i,6))
				csntodec
				print #3, tempstring
	'			print #3, ltrim(str(lines(i,6)))
			End If
		Next
		If datac>0 Then
			Print #8, "'lines data x1,y1,x2,y2"
			Print #8, "DATA ";chr(34);"LINE";Chr(34);",";datac
			For i = 1 To linec
				If lines(i,8)>=0 And lines(i,9)=0 Then
					print #8, "DATA";lines(i,1);",";lines(i,2);",";lines(i,4);",";lines(i,5)
				EndIf
			Next
		EndIf
		'and what is the variable for circles that are blocks
		'circles(i,12)=blocknumber
		datac1=0
		datac2=0
		datac3=0
		For i=1 To circlec
			If circles(i,10)>=0 And circles(i,12)=0 then
				Select Case circles(i,9)
					Case 1
						print #6, "CIRCLE (";circles(i,1);",";circles(i,2);"),";circles(i,4)
						print #7, "CIRCLE (";circles(i,1);" + x1,";circles(i,2);" + y1),";circles(i,4)
						datac1+=1
						print #3, "  0"
						print #3, "CIRCLE"
						print #3, "  5"
						dxfhandlec=dxfhandlec+1
						print #3, Hex$(dxfhandlec)
						print #3, "100"
						print #3, "AcDbEntity"
						print #3, "  8"
						print #3, "MYLAYER"+ltrim(str(circles(i,5)))
						print #3, "100"
						print #3, "AcDbCircle"
						print #3, " 10"
						tempstring=str(circles(i,1))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,1)))
						print #3, " 20"
						tempstring=str(circles(i,2))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,2)))
						print #3, " 30"
						tempstring=str(circles(i,3))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,3)))
						print #3, " 40"
						tempstring=str(circles(i,4))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,4)))
					Case 2
						print #6, "CIRCLE (";circles(i,1);",";circles(i,2);"),";circles(i,4);", ,";
						tempstring=str(circles(i,6))
						csntodec
						print #6, tempstring;", ";
						tempstring=str(circles(i,7))
						csntodec
						print #6, tempstring

						Print #7, "CIRCLE (";circles(i,1);" + x1,";circles(i,2);" + y1),";circles(i,4);", ,";
						tempstring=str(circles(i,6))
						csntodec
						print #7, tempstring;" + arc_start, ";
						tempstring=str(circles(i,7))
						csntodec
						print #7, tempstring;" + arc_end"
						datac2+=1

						print #3, "  0"
						print #3, "ARC"
						print #3, "  5"
						dxfhandlec=dxfhandlec+1
						print #3, Hex$(dxfhandlec)
						print #3, "100"
						print #3, "AcDbEntity"
						print #3, "  8"
						print #3, "MYLAYER"+ltrim(str(circles(i,5)))
						print #3, "100"
						print #3, "AcDbCircle"
						print #3, " 10"
						tempstring=str(circles(i,1))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,1)))
						print #3, " 20"
						tempstring=str(circles(i,2))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,2)))
						print #3, " 30"
						tempstring=str(circles(i,3))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,3)))
						print #3, " 40"
						tempstring=str(circles(i,4))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,4)))
						print #3, "100"
						print #3, "AcDbArc"
						print #3, " 50"
						tempstring=str(circles(i,6)*180/PI)
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,6)*180/PI))
						print #3, " 51"
						tempstring=str(circles(i,7)*180/PI)
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,7)*180/PI))
					Case 3,4
						'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
						'plotellipse(ex1,ey1,ez1,er1,ecolor,estart,eend,er2,eangle)
						print #6, "plotellipse(";circles(i,1);",";circles(i,2);",";circles(i,3);",";circles(i,4);",";circles(i,5);",";circles(i,6);",";circles(i,7);",";circles(i,8);",";circles(i,11);")"
						print #7, "plotellipse(";circles(i,1);" + x1,";circles(i,2);" + y1,";circles(i,3);",";circles(i,4);",";circles(i,5);",";circles(i,6);" + arc_start,";circles(i,7);" + arc_end,";circles(i,8);",";circles(i,11);")"
						datac3+=1

						print #3, "  0"
						print #3, "ELLIPSE"
						print #3, "  5"
						dxfhandlec=dxfhandlec+1
						print #3, Hex$(dxfhandlec)
						print #3, "100"
						print #3, "AcDbEntity"
						print #3, "  8"
						print #3, "MYLAYER"+ltrim(str(circles(i,5)))
						print #3, "100"
						print #3, "AcDbEllipse"
						print #3, " 10"
						tempstring=str(circles(i,1))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,1)))
						print #3, " 20"
						tempstring=str(circles(i,2))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,2)))
						print #3, " 30"
						tempstring=str(circles(i,3))
						csntodec
						print #3, tempstring
						'print #3, ltrim(str(circles(i,3)))
						print #3, " 11"
						fx=cos((circles(i,11))*d2r)*circles(i,4)
						fy=sin((circles(i,11))*d2r)*circles(i,4)
						tempstring=str(fx)
						csntodec
						print #3, tempstring
						print #3, " 21"
						tempstring=str(fy)
						csntodec
						print #3, tempstring
						print #3, " 31"
						print #3, "0"
						print #3, "210"'not sure about these 200 series yet
						print #3, "0.0"
						print #3, "220"
						print #3, "0.0"
						print #3, "230"
						print #3, "1.0"'not sure what value this should be 0, 1 or -1
						print #3, " 40"
						tempstring=str(circles(i,8)/circles(i,4))
						csntodec
						print #3, tempstring
						print #3, " 41"
						tempstring=str(circles(i,6)*d2r)
						csntodec
						print #3, tempstring
						print #3, " 42"
						tempstring=str(circles(i,7)*d2r)
						csntodec
						print #3, tempstring
				End Select
			End If
		Next
		If datac1>0 Then
			Print #8, "'circles data x1,y1,radius"
			Print #8, "DATA ";chr(34);"CIRCLE";Chr(34);",";datac1
			For i=1 To circlec
				If circles(i,10)>=0 And circles(i,12)=0 then
					Select Case circles(i,9)
						Case 1
							print #8, "DATA";circles(i,1);",";circles(i,2);",";circles(i,4)
					End Select
				EndIf
			Next
		EndIf
		If datac2>0 Then
			Print #8, "'arcs data x1,y1,radius,arc_start,arc_end"
			Print #8, "DATA ";chr(34);"ARC";Chr(34);",";datac2
			For i=1 To circlec
				If circles(i,10)>=0 And circles(i,12)=0 then
					Select Case circles(i,9)
						Case 2
							print #8, "DATA";circles(i,1);",";circles(i,2);",";circles(i,4);", ";
							tempstring=str(circles(i,6))
							csntodec
							print #8, tempstring;", ";
							tempstring=str(circles(i,7))
							csntodec
							print #8, tempstring
					End Select
				EndIf
			Next
		EndIf
		If datac3>0 Then
			Print #8, "'ellipse data ex1,ey1,er1,estart,eend,er2,eangle"
			Print #8, "DATA ";chr(34);"ELLIPSE";Chr(34);",";datac3
			For i=1 To circlec
				If circles(i,10)>=0 And circles(i,12)=0 then
					Select Case circles(i,9)
						Case 3,4
							print #8, "DATA";circles(i,1);",";circles(i,2);",";circles(i,4);",";circles(i,6);",";circles(i,7);",";circles(i,8);",";circles(i,11)
					End Select
				EndIf
			Next
		EndIf
		Print #8, "DATA ";Chr(34);"ENTITY DATA END";Chr(34)
		Print #8, "DATA 0"
		Close #6
		Close #7
		Close #8
	End If
End Sub
Sub dxfdictionarys()'active with handles dxf dictionaries
	Print #1, "  0"
	Print #1, "SECTION"
	Print #1, "  2"
	Print #1, "OBJECTS"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "C"
'	Print #1, "330"
'	Print #1, "0"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "  3"
	Print #1, "ACAD_GROUP"
	Print #1, "350"
	Print #1, "D"
	Print #1, "  3"
	Print #1, "ACAD_LAYOUT"
	Print #1, "350"
	Print #1, "1A"
	Print #1, "  3"
	Print #1, "ACAD_MLINESTYLE"
	Print #1, "350"
	Print #1, "17"
	Print #1, "  3"
	Print #1, "ACAD_PLOTSETTINGS"
	Print #1, "350"
	Print #1, "19"
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "65"
'	Print #1, "330"
'	Print #1, "1F"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "  3"
	Print #1, "ACAD_LAYOUTSELFREF"
	Print #1, "350"
	Print #1, Hex$(dxfhandlet)'"EE"'this is post handle entityt+3 ... there are two EE's
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     1"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "63"
'	Print #1, "330"
'	Print #1, "1B"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "  3"
	Print #1, "ACAD_LAYOUTSELFREF"
	Print #1, "350"
	Print #1, Hex$(dxfhandlet-1)'"ED"'this is post handle entityt+2 ... there are two ED's
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     1"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "D"
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "1A"
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "17"
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "  3"
	Print #1, "STANDARD"
	Print #1, "350"
	Print #1, "18"
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "DICTIONARY"
	Print #1, "  5"
	Print #1, "19"
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "C"
	Print #1, "100"
	Print #1, "AcDbDictionary"
	Print #1, "1001"
	Print #1, "ACAD"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "1000"
	Print #1, "TREAT_AS_HARD"
	Print #1, "1070"
	Print #1, "     0"
	Print #1, "  0"
	Print #1, "XRECORD"
	Print #1, "  5"
	Print #1, Hex$(dxfhandlet)'"EE"'this is post handle entityt+3 ... second EE
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "65"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "65"
	Print #1, "100"
	Print #1, "AcDbXrecord"
	Print #1, "340"
	Print #1, "1F"
	Print #1, "  0"
	Print #1, "XRECORD"
	Print #1, "  5"
	Print #1, Hex$(dxfhandlet-1)'"ED"'this is post handle entityt+2 ... second ED
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "63"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "63"
	Print #1, "100"
	Print #1, "AcDbXrecord"
	Print #1, "340"
	Print #1, "1B"
	Print #1, "  0"
	Print #1, "MLINESTYLE"
	Print #1, "  5"
	Print #1, "18"
	Print #1, "102"
	Print #1, "{ACAD_REACTORS"
'	Print #1, "330"
'	Print #1, "17"
	Print #1, "102"
	Print #1, "}"
'	Print #1, "330"
'	Print #1, "17"
	Print #1, "100"
	Print #1, "AcDbMlineStyle"
	Print #1, "  2"
	Print #1, "STANDARD"
	Print #1, " 70"
	Print #1, "     0"
	Print #1, "  3"
	Print #1, ""
	Print #1, " 62"
	Print #1, "   256"
	Print #1, " 51"
	Print #1, "90.0"
	Print #1, " 52"
	Print #1, "90.0"
	Print #1, " 71"
	Print #1, "     2"
	Print #1, " 49"
	Print #1, "0.5"
	Print #1, " 62"
	Print #1, "   256"
	Print #1, "  6"
	Print #1, "BYLAYER"
	Print #1, " 49"
	Print #1, "-0.5"
	Print #1, " 62"
	Print #1, "   256"
	Print #1, "  6"
	Print #1, "BYLAYER"
	Print #1, "  0"
	Print #1, "ENDSEC"
End Sub
sub determinedrawmode2()
	if drawmode=true then
		createline
	else
		beginnewthing
	end if
	mousexp=mousex-1
	tempmousex=mousexp-1
End Sub
sub determinedrawmode1()
	if drawmode=false Then
		If createcircleset=TRUE Then
			'a cirelc, arc, ellipse, elliptical arc was created
			'but the user is still holding the left mouse button down
			'createcircleset goes to false when mouse buttons are released
		Else
			beginnewthing
		EndIf
	Else
		if drawing=true Then
			If buttonson(1)=TRUE Or buttonson(6)=true Then createline
			If buttonson(2)=true Then
				createcircleset=TRUE
				createcircle
			End If
			If buttonson(3)=true Then
				If buttonson(76)=TRUE Then
					buttonson(3)=FALSE
					arcstarted=FALSE
					arcing=FALSE
					mouse_clicks=0
					setdimensiondown
				Else
					createcircleset=TRUE
					createarc
				EndIf
			EndIf
			If buttonson(4)=TRUE Or buttonson(5)=true Then
				createcircleset=TRUE
				createellipse
			End If
			If buttonson(1001)=TRUE Then'Equilateral Triangle
				set_polygon_down(3)
			EndIf
			If buttonson(1002)=TRUE Then'Right Triangle
				
			EndIf
			If buttonson(1003)=TRUE Then'Square
				set_polygon_down(4)
			EndIf
			If buttonson(1004)=TRUE Then'Rectangle
				
			EndIf
			If buttonson(1005)=TRUE Then'Rhombus
				
			EndIf
			If buttonson(1006)=TRUE Then'
				
			EndIf
			If buttonson(1007)=TRUE Then'
				set_polygon_down(5)
			EndIf
			If buttonson(1008)=TRUE Then'
				set_polygon_down(6)
			EndIf
			If buttonson(1009)=TRUE Then'
				set_polygon_down(7)
			EndIf
			If buttonson(1010)=TRUE Then'Octogon
				set_polygon_down(8)
			EndIf
		End If 
	end if
	mousexp=mousex-1
	tempmousex=mousexp-1
End Sub
sub createline()
	If drawatangle=true then
		lastangle=angle2
	else
		lastangle=angle
	end if
	memmanageline
	lines(linec,1)=x1
	lines(linec,2)=y1
	If buttonson(6)=TRUE Then
		splinec=splinec+1
		splinepoint(splinec)=linec
	EndIf
	if forcex=true Then
		x2=fx
		forcex=false
		fx=0
	end if
	if forcey=true then
		y2=fy
		forcey=false
		fy=0
	end if
	lines(linec,4)=x2
	lines(linec,5)=y2
	lines(linec,7)=lc
'	screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	if x1=x2 and y1=y2 then
		linec=linec-1
	Else
		''ScreenCopy 0,1
		'updatefbgfxgtkimage=TRUE
		'ScreenSet 1,1
		If buttonson(6)=TRUE Then
			If splinec>1 Then
				Dim As Double aol1,aol2
				selline=splinepoint(splinec-1)
				calcselangle()
				aol1=selangle
				selline=splinepoint(splinec)
				calcselangle()
				aol2=selangle
				If aol1=aol2 Then
					'merge the two lines into one
					lines(splinepoint(splinec-1),4)=x2
					lines(splinepoint(splinec-1),5)=y2
					linec=linec-1
					splinec=splinec-1
					line (lines(splinepoint(splinec),1),lines(splinepoint(splinec),2))-(x2,y2),lc
				Else
					line (x1,y1)-(x2,y2),lc
				EndIf
			EndIf
		Else
			line (x1,y1)-(x2,y2),lc
		End If
	end If
	x1=x2
	y1=y2
	theboxbelow("New line created - Line #"+Str(linec-preloadedlinec))
	drawing=false
	initlinedraw
End Sub
Sub createcircle()
'Circle (x1+(x2-x1)/2,y1+(y2-y1)/2),Sqr((x1-x2)^2 + (y1-y2)^2)/2,31
	memmanagecircle
	Select Case drawingcirclesmethod
		Case 1
			Select Case shift_key
				Case 0
					
				Case 1,2
					length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
					x1=x1+(x2-x1)/2
					y1=y1+(y2-y1)/2
			End Select
		Case 2
			Select Case shift_key
				Case 1,2
					
				Case 0
					length=Sqr((x1-x2)^2 + (y1-y2)^2)/2
					x1=x1+(x2-x1)/2
					y1=y1+(y2-y1)/2
			End Select
	End Select
	circles(circlec,1)=x1
	circles(circlec,2)=y1

	circles(circlec,4)=length
	circles(circlec,5)=lc
	circles(circlec,9)=1
'	screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	if x1=x2 and y1=y2 then
		circlec=circlec-1
	else
		circle(x1,y1),length,lc
	end If
	
	x1=x2
	y1=y2
	theboxbelow("New circle created - Circle #"+Str(circlec-preloadedcirclec))
	drawing=false
	snap=false
	initlinedraw
End Sub
Sub createarc()

	Dim i As Integer
	memmanagecircle
	circles(circlec,1)=x1
	circles(circlec,2)=y1

	circles(circlec,4)=radius
	circles(circlec,5)=lc
	circles(circlec,6)=arcstart
	circles(circlec,7)=arcend
	circles(circlec,9)=2
	if x1=x2 and y1=y2 then
		circlec=circlec-1
	else
		circle(x1,y1),radius,lc,arcstart,arcend
	end If
	x1=x2
	y1=y2
	drawing=false
	snap=false
	initlinedraw
End Sub
sub createnewmodlines()
	memmanageline
	lines(linec,1)=x1
	lines(linec,2)=y1
	lines(linec,4)=x2
	lines(linec,5)=y2
	lines(linec,7)=lc
	lines(linec,8)=0
End Sub
sub createnewmodcircles(j As Integer)
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
	memmanagecircle
	circles(circlec,1)=x1
	circles(circlec,2)=y1
	circles(circlec,3)=z1
	circles(circlec,4)=radius
	circles(circlec,5)=lc
	circles(circlec,6)=arcstart
	circles(circlec,7)=arcend
	circles(circlec,8)=aspect
	circles(circlec,9)=j
	circles(circlec,10)=0
	circles(circlec,11)=erotation
End Sub
Sub memmanageviews()
	viewsc=viewsc+1
	If viewsc>viewsmemstep Then
		viewsmemstep=viewsmemstep+10
		ReDim Preserve views(viewsmemstep,4)
	EndIf
End Sub
sub memmanageline()
	linec=linec+1
	If linec>linememstep Then
		linememstep=linememstep+1000
		ReDim Preserve lines(linememstep,9)
		ReDim inviewlines(linememstep)
		ReDim inviewfilterlines(linememstep)
	End If
End Sub
Sub memmanagecircle()
	circlec=circlec+1
	If circlec>circlememstep Then
		circlememstep=circlememstep+1000
		ReDim Preserve circles(circlememstep,12)
		ReDim inviewcircles(circlememstep)
		ReDim inviewfiltercircles(circlememstep)
	End If
End Sub
sub beginnewthing()
	drawmode=true
	mousexp=0
	'if selentity=true and snapenable=true then
	if selentity=TRUE then
		x1=fxm
		y1=fym
		If buttonson(53)=TRUE Or buttonson(54)=TRUE Then
			fromotd=otd
			forcex=TRUE
			forcey=TRUE
			drawatangle=TRUE
			Select Case otd
				Case "line"
					perpangle=selangle
				Case "circle","arc"
					xlength=fxm-circles(selcircle,1)
					ylength=fym-circles(selcircle,2)
					fixperpangle
				Case "ellipse","elliptical arc"
					perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
					If perpangle<0 then perpangle=360+perpangle
			End Select
		EndIf
	Else
		If forcex=TRUE Then
			x1=fx
		Else
			x1=mousex
		End If
		If forcey=TRUE Then
			y1=fy
		Else
			y1=mousey
		End If
		forcex=FALSE
		forcey=FALSE
	end If
End Sub
Sub movepoints()
	Dim As Integer i
	movingpoints=false
	If buttonson(51)=true then
		Select Case mousex
			Case fxm-detect To fxm+detect
				Select Case mousey
					Case fym-detect To fym+detect
						If groupexists=TRUE Then
							For i = 1 To linec
								If lines(i,8)=1 Then
									If lines(i,1)=fxm Or lines(i,4)=fxm Then
										If lines(i,2)=fym Or lines(i,5)=fym Then
											Line (fxm-detect+3,fym-detect+3)-(fxm+detect-3,fym+detect-3),12,bf
											movingpoints=TRUE
											mpfxm=fxm
											mpfym=fym
											Exit for
										EndIf
									EndIf
								EndIf
							Next
						Else
							Line (fxm-detect+3,fym-detect+3)-(fxm+detect-3,fym+detect-3),12,bf
							movingpoints=TRUE
							mpfxm=fxm
							mpfym=fym
						EndIf
				End Select
		End Select
	End if
End Sub
Sub movepointstomouse()
	Dim As Integer i
	For i = 1 To linec
		If groupexists=TRUE Then
			If lines(i,8)=1 Then
				If lines(i,1)=mpfxm And lines(i,2)=mpfym Then
					'lines(i,1)=mousex And lines(i,2)=mousey
					Line (modifyx2,modifyy2)-(lines(i,4),lines(i,5)),31
				EndIf
				If lines(i,4)=mpfxm And lines(i,5)=mpfym Then
					'lines(i,4)=mousex And lines(i,5)=mousey
					Line (modifyx2,modifyy2)-(lines(i,1),lines(i,2)),31
				EndIf
			End If
		Else
			If lines(i,1)=mpfxm And lines(i,2)=mpfym Then
				'lines(i,1)=mousex And lines(i,2)=mousey
				Line (modifyx2,modifyy2)-(lines(i,4),lines(i,5)),31
			EndIf
			If lines(i,4)=mpfxm And lines(i,5)=mpfym Then
				'lines(i,4)=mousex And lines(i,5)=mousey
				Line (modifyx2,modifyy2)-(lines(i,1),lines(i,2)),31
			EndIf
		EndIf
	Next
End Sub
Sub setmovepointsdown()
	Dim As Integer i
	
	For i = 1 To linec
		If groupexists=TRUE Then
			If lines(i,8)=1 Then
				If lines(i,1)=mpfxm And lines(i,2)=mpfym Then
					lines(i,1)=modifyx2
					lines(i,2)=modifyy2
					'Line (mousex,mousey)-(lines(i,4),lines(i,5)),31
				EndIf
				If lines(i,4)=mpfxm And lines(i,5)=mpfym Then
					lines(i,4)=modifyx2
					lines(i,5)=modifyy2
					'Line (mousex,mousey)-(lines(i,1),lines(i,2)),31
				EndIf
			End If
		Else
			If lines(i,1)=mpfxm And lines(i,2)=mpfym Then
				lines(i,1)=modifyx2
				lines(i,2)=modifyy2
				'Line (mousex,mousey)-(lines(i,4),lines(i,5)),31
			EndIf
			If lines(i,4)=mpfxm And lines(i,5)=mpfym Then
				lines(i,4)=modifyx2
				lines(i,5)=modifyy2
				'Line (mousex,mousey)-(lines(i,1),lines(i,2)),31
			End if
		End if
	Next
End Sub
Sub scanpoints()
	Dim detectwindow As Integer
	detectwindow=5
	dp=false
	For dpmx=Int(scrnmx)-detectwindow To Int(scrnmx)+detectwindow
		For dpmy=Int(scrnmy)-detectwindow to Int(scrnmy)+detectwindow
			pp=point(dpmx,dpmy)
			Select case pp
				case 1 To 15,34
					dp=TRUE
					Exit sub
			End Select
		Next
	Next
End Sub
Sub detectpoints()
	selline=0
	selcircle=0
	selentity=FALSE
	otd=""
	If groupexists=TRUE And buttonson(109)=TRUE And groupscaling=FALSE Then
		Dim As Integer tempdetect
		tempdetect=detect
		detect=detect*2
		groupgrab=FALSE
		Select Case mousex
			Case gextentsx1-detect To gextentsx1+detect'left side
				Select Case mousey
					Case gextentsy1-detect To gextentsy1+detect'bottom left
						groupgrabhandle="bottom left"
						groupgrab=TRUE
					Case gextentsy1+(gextentsy2-gextentsy1)/2-detect To gextentsy1+(gextentsy2-gextentsy1)/2+detect'left side mid
						groupgrabhandle="left side mid"
						groupgrab=TRUE
					Case gextentsy2-detect To gextentsy2+detect'top left
						groupgrabhandle="top left"
						groupgrab=TRUE
				End Select
			Case gextentsx1+(gextentsx2-gextentsx1)/2-detect To gextentsx1+(gextentsx2-gextentsx1)/2+detect'mid x
				Select Case mousey
					Case gextentsy1-detect To gextentsy1+detect'bottom mid
						groupgrabhandle="bottom mid"
						groupgrab=TRUE
					Case gextentsy2-detect To gextentsy2+detect'top mid
						groupgrabhandle="top mid"
						groupgrab=TRUE
				End Select
			Case gextentsx2-detect To gextentsx2+detect'right side
				Select Case mousey
					Case gextentsy1-detect To gextentsy1+detect'bottom right
						groupgrabhandle="bottom right"
						groupgrab=TRUE
					Case gextentsy1+(gextentsy2-gextentsy1)/2-detect To gextentsy1+(gextentsy2-gextentsy1)/2+detect'right side mid
						groupgrabhandle="right side mid"
						groupgrab=TRUE
					Case gextentsy2-detect To gextentsy2+detect'top right
						groupgrabhandle="top right"
						groupgrab=TRUE
				End Select
		End Select
		detect=tempdetect
		Exit Sub 
	EndIf
	If snapenable=FALSE Or buttonson(43)=TRUE Then Exit Sub
	Dim otd1 As String 'first object type detected
	Dim otd2 As String 'first object type detected
	Dim teotd As String ' trim extend object type detected
	Dim As Double fxmt,fymt
	Dim As Byte tespset
	Dim As Double aia
	Dim As Integer i,j,ij,ivi,k,ivk
	Dim As Double erot,cr1,cr2
	aintersect=-1
	Dim As String nearest_entity,second_nearest_entity
	otd="line"
	Dim As Integer nearesti,second_nearesti
	nearesti=-1
	second_nearesti=-1
	Dim As Double nearestd,second_nearestd
	nearestd=0
	second_nearestd=0
	for ivi = inviewfilterlinesc to 1 step -1
		i=inviewfilterlines(ivi)
		Select Case layerstate(lines(i,7))
			Case 2
				if lines(i,8)=0 Or lines(i,8)=1 Then
					selline=i
					calcselangle
					calcsellength
					calcnearestpoint
					'gtk_text_buffer_set_text (textinfobuffer, intersection, -1)
					If nearesti=-1 Then
						nearesti=i
						nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
						nearest_entity=otd
						'second_nearesti=nearesti
						'second_nearestd=nearestd
						'second_nearest_entity=otd
					Else
						If sqr((fxm-mousex)^2 + (fym-mousey)^2)<nearestd Then
							second_nearesti=nearesti
							second_nearestd=nearestd
							second_nearest_entity=otd
							nearesti=i
							nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
							nearest_entity=otd
						Else
							If second_nearest_entity="" Then
								second_nearesti=i
								second_nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
								second_nearest_entity=otd
							Else
								If sqr((fxm-mousex)^2 + (fym-mousey)^2)<second_nearestd Then
									second_nearesti=i
									second_nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
									second_nearest_entity=otd
								EndIf
							EndIf
						EndIf
					EndIf
				EndIf
		End Select
	Next
	for ivi = inviewfiltercirclesc to 1 step -1
		i=inviewfiltercircles(ivi)
		Select case layerstate(circles(i,5))
			Case 2
				If circles(i,10)=0 Or circles(i,10)=1 Then
					Select Case circles(i,9)
						Case 1
							otd="circle"
						Case 2
							otd="arc"
						Case 3
							otd="ellipse"
						Case 4
							otd="elliptical arc"
					End Select
					selcircle=i
					calcnearestpoint
					If nearesti=-1 Then
						nearesti=i
						nearestd=Sqr((fxm-mousex)^2 + (fym-mousey)^2)
						nearest_entity=otd
					Else
						If sqr((fxm-mousex)^2 + (fym-mousey)^2)<nearestd Then
							second_nearesti=nearesti
							second_nearestd=nearestd
							second_nearest_entity=nearest_entity
							nearesti=i
							nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
							nearest_entity=otd
						Else
							If second_nearest_entity="" Then
									second_nearesti=i
									second_nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
									second_nearest_entity=otd
							Else
									If sqr((fxm-mousex)^2 + (fym-mousey)^2)<second_nearestd Then
										second_nearesti=i
										second_nearestd=sqr((fxm-mousex)^2 + (fym-mousey)^2)
										second_nearest_entity=otd
									EndIf
							EndIf
						EndIf
					EndIf
				EndIf
		End Select
	Next
	If nearesti>0 And nearestd<detect*2 Then
		If buttonson(18)=TRUE Then
			If second_nearest_entity<>"" Then
				Select Case nearest_entity
					Case "line"
						aintersect=nearesti
						bintersect=second_nearesti
						selentity=true
						Select Case second_nearest_entity
							Case "line"
								calclinelineintersection
							Case "circle","arc"
								calclinecircleintersection
							Case "ellipse","elliptical arc"
								calclineellipseintersection
						End Select
					Case "circle","arc"
						aintersect=second_nearesti
						bintersect=nearesti
						selentity=true
						Select Case second_nearest_entity
							Case "line"
								calclinecircleintersection
							Case "circle","arc"
								calccirclecircleintersection
							Case "ellipse","elliptical arc"
								calccircleellipseintersection
						End Select
					Case "ellipse","elliptical arc"
						aintersect=second_nearesti
						bintersect=nearesti
						selentity=true
						Select Case second_nearest_entity
							Case "line"
								calclineellipseintersection
							Case "circle","arc"
								calccircleellipseintersection
							Case "ellipse","elliptical arc"
								calcellipseellipseintersection
						End Select
				End Select
				Line (fxm-detect,fym-detect)-(fxm+detect,fym+detect),14,b
			EndIf
		Else
			otd=nearest_entity
			i=nearesti
			selentity=true
			Select Case otd
				Case "line"
					selcircle=0
					selline=i
					calcselangle
					calcsellength
					If buttonson(11)=true Or buttonson(12)=TRUE Or buttonson(13)=TRUE Or buttonson(19)=true Then
						If lines(i,9)>0 Then hiliblock(CInt(lines(i,9)))
						'selline=i
						'selentity=true
						'calcselangle
						'calcsellength
						If buttonson(11)=true Then
							If calcd(lines(i,1),lines(i,2),lines(i,3),cdbl(mousex),cdbl(mousey),cdbl(mousez)) < calcd(lines(i,4),lines(i,5),lines(i,6),cdbl(mousex),cdbl(mousey),cdbl(mousez)) Then
								fxm=lines(i,1)
								fym=lines(i,2)
								if forcex=true and fx=0 then fx=fxm
								if forcey=true and fy=0 then fy=fym
							Else
								fxm=lines(i,4)
								fym=lines(i,5)
								if forcex=true and fx=0 then fx=fxm
								if forcey=true and fy=0 then fy=fym
							EndIf
						EndIf
						If buttonson(12)=true Then calcmidpoint
						If buttonson(13)=true Then calcperpendicular
						If buttonson(19)=true Then calcnearestpoint
						Line (fxm-detect,fym-detect)-(fxm+detect,fym+detect),14,b
					EndIf
				Case "circle","arc","ellipse","elliptical arc"
					selline=0
					selcircle=i
					If buttonson(11)=true Or buttonson(12)=TRUE Or buttonson(14)=TRUE Or buttonson(16)=true  Or buttonson(17)=TRUE Or buttonson(19)=true Then
						If circles(i,12)>0 Then
							hiliblock(CInt(circles(i,12)))
						EndIf
						if buttonson(11)=true Then
							Select Case circles(i,9)
								Case 1,3
									fxm=circles(i,1)
									fym=circles(i,2)
								Case 2
									calcarcendpoints
									If calcd(arcendpoint1x,arcendpoint1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(arcendpoint2x,arcendpoint2y,0,CDbl(mousex),CDbl(mousey),0) Then
										fxm=arcendpoint1x
										fym=arcendpoint1y
									Else
										fxm=arcendpoint2x
										fym=arcendpoint2y
									EndIf
								Case 4
									calcellipsarcendpoints(i)
									If calcd(arcendpoint1x,arcendpoint1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(arcendpoint2x,arcendpoint2y,0,CDbl(mousex),CDbl(mousey),0) Then
										fxm=arcendpoint1x
										fym=arcendpoint1y
									Else
										fxm=arcendpoint2x
										fym=arcendpoint2y
									EndIf
							End Select
							if forcex=true and fx=0 then fx=fxm
							If forcey=true and fy=0 then fy=fym
						End If
						if buttonson(12)=TRUE Then
							Select Case circles(i,9)
								Case 2
									calcarcmidpoint
								Case 4
									calcellipsarcmidpoint(i)
							End select
							fxm=arcmidpointx
							fym=arcmidpointy
						EndIf
						If buttonson(14)=TRUE Then
							'calctangentpoint
							If buttonson(1)=TRUE Or buttonson(6)=TRUE Then
								'is a line being drawn
								if drawing=TRUE Then
									calctangentpoint
									'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
									Select Case circles(i,9)
										Case 1
										Case 2
											'abort if fxm,fym is not on arc
											memmanageline
											lines(linec,1)=circles(selcircle,1)
											lines(linec,2)=circles(selcircle,2)
											lines(linec,4)=fxm
											lines(linec,5)=fym
											selline=linec
											calcselangle
											linec=linec-1
											'radians=degrees*d2r
											Dim radians As Double
											radians=selangle*d2r
											If circles(selcircle,6)>circles(selcircle,7) Then
												Select case radians
													Case 0 To circles(selcircle,7), circles(selcircle,6) To 360*d2r
														'it's in the arc
													Case Else
														'abort
														fxm=circles(selcircle,1)
														fym=circles(selcircle,2)
												End Select
											Else
												Select case radians
													Case circles(selcircle,6) To circles(selcircle,7)
														'it's in the arc
													Case Else
														'abort
														fxm=circles(selcircle,1)
														fym=circles(selcircle,2)
												End Select
											EndIf
										Case 3
										Case 4
									End Select												
								End If
							End If
						End If
						if buttonson(16)=TRUE Or buttonson(17)=TRUE Then
							fxm=circles(i,1)
							fym=circles(i,2)
						EndIf
						If buttonson(19)=TRUE Then
							calcnearestpoint
						EndIf
						calcselangle
						calcsellength
						Line (fxm-detect,fym-detect)-(fxm+detect,fym+detect),14,b
					EndIf
			End Select
		EndIf
		If buttonson(61)=TRUE Or buttonson(62)=TRUE Then
			teotd=nearest_entity
			If buttonson(61)=TRUE Then 'show trim
				tesptype=teotd
				Select Case teotd
					Case "line"
						'does this line intersect multiple objects
						'and if so, define the starting and ending points
						'of the segment(s) of the line to trim
						selline=nearesti
						aintersect=selline
						multic=0
						for ivi = inviewfilterlinesc to 1 step -1
							i=inviewfilterlines(ivi)
							If lines(i,8)=1 And i<>aintersect Then
								bintersect=i
								calclinelineintersection
								If intersection="inside" Then
									multic=multic+1
									ReDim Preserve multifxm(multic)
									ReDim Preserve multifym(multic)
									multifxm(multic)=fxm
									multifym(multic)=fym
								EndIf
							EndIf
						Next
						for ivi = inviewfiltercirclesc to 1 step -1
							i=inviewfiltercircles(ivi)
							If circles(i,10)=1 then
								bintersect=i
								Select Case circles(i,9)
									Case 1,2
										calclinecircleintersection
									Case 3,4
										calclineellipseintersection
								End Select
								If intersection="inside" Then
									Select Case clic
										Case 1
											multic=multic+1
											ReDim Preserve multifxm(multic)
											ReDim Preserve multifym(multic)
											multifxm(multic)=clifxm1
											multifym(multic)=clifym1
										Case 2
											multic=multic+1
											ReDim Preserve multifxm(multic)
											ReDim Preserve multifym(multic)
											multifxm(multic)=clifxm1
											multifym(multic)=clifym1
											multic=multic+1
											ReDim Preserve multifxm(multic)
											ReDim Preserve multifym(multic)
											multifxm(multic)=clifxm2
											multifym(multic)=clifym2
									End Select
									
								End If
							End If
						Next
						Select case multic
							Case Is > 0
								multic = multic + 1
								ReDim Preserve multifxm(multic)
								ReDim Preserve multifym(multic)
								multifxm(multic)=lines(aintersect,1)
								multifym(multic)=lines(aintersect,2)
								multic = multic + 1
								ReDim Preserve multifxm(multic)
								ReDim Preserve multifym(multic)
								multifxm(multic)=lines(aintersect,4)
								multifym(multic)=lines(aintersect,5)
								'which two intersections is the mouse closest to
								Dim As Double mtop,mtopp,mtopmax
								Dim As Integer mtopi1,mtopi2
								mtopp=0
								mtopi1=0
								mtopi2=0
								For i = 1 To multic
									Circle (multifxm(i),multifym(i)),4
									mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
									If mtop > mtopp  Then mtopp=mtop
								Next
								mtopmax=mtopp'this is the farthest distance mouse to intersection
								For i = 1 To multic
									mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
									If mtop < mtopp  Then
										mtopp=mtop
										mtopi1=i
									End If
								Next
								'which side of mtopi1 is the mouse on
								If Abs(lines(aintersect,1)-lines(aintersect,4))>Abs(lines(aintersect,2)-lines(aintersect,5)) Then
									If mousex > multifxm(mtopi1) Then
										'find the next intersection that is Greater then
										'mtopi1 and closest to the mouse
										mtopp=mtopmax
										For i = 1 To multic
											If i <> mtopi1 And multifxm(i)>multifxm(mtopi1) then
												mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
												If mtop < mtopp  Then
													mtopp=mtop
													mtopi2=i
												End If
											End if
										Next
									Else
										'find the next intersection that is Less then
										'mtopi1 and closest to it
										mtopp=mtopmax
										For i = 1 To multic
											If i <> mtopi1 And multifxm(i)<multifxm(mtopi1) then
												mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
												If mtop < mtopp  Then
													mtopp=mtop
													mtopi2=i
												End If
											End if
										Next
									End If
								Else
									If mousey > multifym(mtopi1) Then
										'find the next intersection that is Greater then
										'mtopi1 and closest to the mouse
										mtopp=mtopmax
										For i = 1 To multic
											If i <> mtopi1 And multifym(i)>multifym(mtopi1) then
												mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
												If mtop < mtopp  Then
													mtopp=mtop
													mtopi2=i
												End If
											End if
										Next
									Else
										'find the next intersection that is Less then
										'mtopi1 and closest to it
										mtopp=mtopmax
										For i = 1 To multic
											If i <> mtopi1 And multifym(i)<multifym(mtopi1) then
												mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
												If mtop < mtopp  Then
													mtopp=mtop
													mtopi2=i
												End If
											End if
										Next
									End If
								End If
								If mtopi2 <> 0 Then
									tesp=0
									trimlinei=aintersect
									Line (multifxm(mtopi1),multifym(mtopi1)) - (multifxm(mtopi2),multifym(mtopi2)),10
									If multifxm(mtopi1)=lines(aintersect,1) And multifym(mtopi1)=lines(aintersect,2) Then
										tesp=1
										ipointsx(1)=multifxm(mtopi2)
										ipointsy(1)=multifym(mtopi2)
										ipointsx(2)=lines(aintersect,4)
										ipointsy(2)=lines(aintersect,5)
									Else
										If multifxm(mtopi1)=lines(aintersect,4) And multifym(mtopi1)=lines(aintersect,5) Then
											tesp=1
											ipointsx(1)=lines(aintersect,1)
											ipointsy(1)=lines(aintersect,2)
											ipointsx(2)=multifxm(mtopi2)
											ipointsy(2)=multifym(mtopi2)
										Else
											If sqr((lines(aintersect,1)-multifxm(mtopi1))^2 + (lines(aintersect,2)-multifym(mtopi1))^2) < sqr((lines(aintersect,1)-multifxm(mtopi2))^2 + (lines(aintersect,2)-multifym(mtopi2))^2) Then
												tesp=2
												ipointsx(1)=lines(aintersect,1)
												ipointsy(1)=lines(aintersect,2)
												ipointsx(2)=multifxm(mtopi1)
												ipointsy(2)=multifym(mtopi1)
												ipointsx(3)=multifxm(mtopi2)
												ipointsy(3)=multifym(mtopi2)
												ipointsx(4)=lines(aintersect,4)
												ipointsy(4)=lines(aintersect,5)
											Else
												tesp=2
												ipointsx(1)=lines(aintersect,1)
												ipointsy(1)=lines(aintersect,2)
												ipointsx(2)=multifxm(mtopi2)
												ipointsy(2)=multifym(mtopi2)
												ipointsx(3)=multifxm(mtopi1)
												ipointsy(3)=multifym(mtopi1)
												ipointsx(4)=lines(aintersect,4)
												ipointsy(4)=lines(aintersect,5)
											End If
										End If
									End if
								End If
						End select
					Case "circle","arc","ellipse","elliptical arc"
						selcircle=nearesti
						aintersect=selcircle
						tci=selcircle
						clic=0
						multic=0
						If teotd="arc" Or teotd="elliptical arc" Then
							multic=2
							ReDim Preserve multifxm(multic)
							ReDim Preserve multifym(multic)
							multifxm(1)=arcendpoint1x
							multifym(1)=arcendpoint1y
							multifxm(2)=arcendpoint2x
							multifym(2)=arcendpoint2y
						End If
						for ivi = inviewfilterlinesc to 1 step -1
							i=inviewfilterlines(ivi)
							If lines(i,8)=1 then
								bintersect=tci
								aintersect=i
								Select Case circles(tci,9)
									Case 1,2
										calclinecircleintersection
									Case 3,4
										calclineellipseintersection
								End Select
								If intersection="inside" Then
									Select Case clic
										Case 1
											multic=multic+1
											ReDim Preserve multifxm(multic)
											ReDim Preserve multifym(multic)
											multifxm(multic)=clifxm1
											multifym(multic)=clifym1
										Case 2
											multic=multic+1
											ReDim Preserve multifxm(multic)
											ReDim Preserve multifym(multic)
											multifxm(multic)=clifxm1
											multifym(multic)=clifym1
											multic=multic+1
											ReDim Preserve multifxm(multic)
											ReDim Preserve multifym(multic)
											multifxm(multic)=clifxm2
											multifym(multic)=clifym2
									End Select
									
								EndIf
							End If
						Next
						
						Select Case teotd
							Case "circle","arc"
								aintersect=tci
								for ivi = inviewfiltercirclesc to 1 step -1
									i=inviewfiltercircles(ivi)
									If i <> tci And circles(i,10)=1  Then
										Select case circles(i,9)
											Case 1,2'circle, arc
												bintersect=i
												intersection=""
												calccirclecircleintersection
												If intersection="inside" Then
													Select Case ccic
														Case 1
														multic=multic+1
														ReDim Preserve multifxm(multic)
														ReDim Preserve multifym(multic)
														multifxm(multic)=clifxm1
														multifym(multic)=clifym1
														Case 2
														multic=multic+1
														ReDim Preserve multifxm(multic)
														ReDim Preserve multifym(multic)
														multifxm(multic)=clifxm1
														multifym(multic)=clifym1
														multic=multic+1
														ReDim Preserve multifxm(multic)
														ReDim Preserve multifym(multic)
														multifxm(multic)=clifxm2
														multifym(multic)=clifym2
													End Select
												EndIf
											Case 3,4
												mousextemp=mousex
												mouseytemp=mousey
												bintersect=i
												ccic=0
												For j=0 To 350 Step 10'degrees
													x1p=(circles(i,1)+(Cos(j*d2r)*circles(i,4)))-circles(i,1)
													y1p=(circles(i,2)+(Sin(j*d2r)*circles(i,8)))-circles(i,2)
													mousex=x1p*Cos(circles(i,11)*d2r) - y1p*Sin(circles(i,11)*d2r)+circles(i,1)
													mousey=y1p*Cos(circles(i,11)*d2r) + x1p*Sin(circles(i,11)*d2r)+circles(i,2)
													calccircleellipseintersectionold '********
													'Line(mousex,mousey)-(fxm,fym)
													If ipass=1 Then
														For ij=1 To multic
															If multifxm(ij)=fxm And multifym(ij)=fym Then
																ipass=0
																Exit for
															EndIf
														Next
														If ipass=1 Then
															multic=multic+1
															ReDim Preserve multifxm(multic)
															ReDim Preserve multifym(multic)
															multifxm(multic)=fxm
															multifym(multic)=fym
															ccic=ccic+1
														EndIf
													EndIf
													If ccic=4 Then
														Exit for
													EndIf
												Next
												mousex=mousextemp
												mousey=mouseytemp
										End select
									EndIf
								Next
								'get the angle of mouse to circle
								xlength=mousex-circles(tci,1)
								ylength=mousey-circles(tci,2)
								fixangle
								aia=PI*angle/180
								
							Case "ellipse","elliptical arc"
								'here is where to check for other circles
								mousextemp=mousex
								mouseytemp=mousey
								aintersect=tci
								for ivi = inviewfiltercirclesc to 1 step -1
									i=inviewfiltercircles(ivi)
									If i <> tci And circles(i,10)=1  Then
										bintersect=i
										intersection=""
										Select case circles(i,9)
											Case 1,2
												ccic=0
												For j=0 To 350 Step 10'degrees
													x1p=(circles(tci,1)+(Cos(j*d2r)*circles(tci,4)))-circles(tci,1)
													y1p=(circles(tci,2)+(Sin(j*d2r)*circles(tci,8)))-circles(tci,2)
													mousex=x1p*Cos(circles(tci,11)*d2r) - y1p*Sin(circles(tci,11)*d2r)+circles(tci,1)
													mousey=y1p*Cos(circles(tci,11)*d2r) + x1p*Sin(circles(tci,11)*d2r)+circles(tci,2)
													calccircleellipseintersectionold '********
													If ipass=1 Then
														For ij=1 To multic
															If multifxm(ij)=fxm And multifym(ij)=fym Then
																ipass=0
																Exit for
															EndIf
														Next
														If ipass=1 Then
															multic=multic+1
															ReDim Preserve multifxm(multic)
															ReDim Preserve multifym(multic)
															multifxm(multic)=fxm
															multifym(multic)=fym
															ccic=ccic+1
														EndIf
													EndIf
													If ccic=4 Then
														Exit for
													EndIf
												Next
												mousex=mousextemp
												mousey=mouseytemp
											Case 3,4
												ccic=0
												For j=0 To 350 Step 10'degrees
													x1p=(circles(tci,1)+(Cos(j*d2r)*circles(tci,4)))-circles(tci,1)
													y1p=(circles(tci,2)+(Sin(j*d2r)*circles(tci,8)))-circles(tci,2)
													mousex=x1p*Cos(circles(tci,11)*d2r) - y1p*Sin(circles(tci,11)*d2r)+circles(tci,1)
													mousey=y1p*Cos(circles(tci,11)*d2r) + x1p*Sin(circles(tci,11)*d2r)+circles(tci,2)
													calcellipseellipseintersectionold '********
													If ipass=1 Then
														For ij=1 To multic
															If multifxm(ij)=fxm And multifym(ij)=fym Then
																ipass=0
																Exit for
															EndIf
														Next
														If ipass=1 Then
															multic=multic+1
															ReDim Preserve multifxm(multic)
															ReDim Preserve multifym(multic)
															multifxm(multic)=fxm
															multifym(multic)=fym
															ccic=ccic+1
														EndIf
													EndIf
													If ccic=4 Then
														Exit for
													EndIf
												Next
												mousex=mousextemp
												mousey=mouseytemp
										End Select
									EndIf
								Next
								cr1=circles(tci,4)
								cr2=circles(tci,8)
								erot=circles(tci,11)
								x1=circles(tci,1)
								y1=circles(tci,2)
								x2=mousex
								y2=mousey
								x1p=x2-x1
								y1p=(y2-y1)
								x2=x1p*Cos(-erot*d2r) - y1p*Sin(-erot*d2r)+x1
								y2=y1p*Cos(-erot*d2r) + x1p*Sin(-erot*d2r)+y1
								ylength=(y2-y1)*(cr1/cr2)
								xlength=x2-x1
								fixangle
								aia=angle							
						End Select
						' modify this to consider ellipses
						' i think aia is in radians (not degrees)
						Dim As Double mtotcia
						mtotcia=aia
						Dim As Integer ciatomtotciai1,ciatomtotciai2,ciatomtotciac,ciatomtotciaimin,ciatomtotciaimax
						Dim As Double ciatomtotcia1,ciatomtotcia2,ciatomtotciamin,ciatomtotciamax
						ciatomtotciac=0
						ciatomtotciai1=0
						ciatomtotciai2=0
						ciatomtotcia1=mtotcia
						ciatomtotcia2=mtotcia
						
						ciatomtotciaimin=0
						ciatomtotciaimax=0
						ciatomtotciamin=mtotcia
						ciatomtotciamax=mtotcia

						For i = 1 To multic
							Select Case teotd
								Case "circle","arc"
									xlength=multifxm(i)-circles(tci,1)
									ylength=multifym(i)-circles(tci,2)
									fixangle
									aia=PI*angle/180
								Case "ellipse","elliptical arc"
									'get the angle of mouse to ellipse
									'take into consideration erot
									cr1=circles(tci,4)
									cr2=circles(tci,8)
									erot=circles(tci,11)
									x1=circles(tci,1)
									y1=circles(tci,2)
									x2=multifxm(i)
									y2=multifym(i)
									x1p=x2-x1
									y1p=(y2-y1)
									x2=x1p*Cos(-erot*d2r) - y1p*Sin(-erot*d2r)+x1
									y2=y1p*Cos(-erot*d2r) + x1p*Sin(-erot*d2r)+y1
									ylength=(y2-y1)*(cr1/cr2)
									xlength=x2-x1
									fixangle
									aia=angle
									'maybe try radians
									'aia=PI*angle/180
							End Select
							
							
							
							Circle (multifxm(i),multifym(i)),4
							If aia < ciatomtotciamin Then
								ciatomtotciamin=aia
								ciatomtotciaimin=i
							End If
							If aia > ciatomtotciamax Then
								ciatomtotciamax=aia
								ciatomtotciaimax=i
							End If

							If aia<ciatomtotcia1 And ciatomtotciai1=0 Then
								ciatomtotcia1=aia
								ciatomtotciai1=i
							Else
								If aia<mtotcia And aia>ciatomtotcia1 Then
									ciatomtotcia1=aia
									ciatomtotciai1=i
									ciatomtotciac=ciatomtotciac+1
								EndIf
							EndIf
							
							If aia>ciatomtotcia2 And ciatomtotciai2=0 Then
								ciatomtotcia2=aia
								ciatomtotciai2=i
							Else
								If aia>mtotcia And aia<ciatomtotcia2 Then
									ciatomtotcia2=aia
									ciatomtotciai2=i
								EndIf
							EndIf
							'mtop=sqr((mousex-multifxm(i))^2 + (mousey-multifym(i))^2)
							'If mtop > mtopp  Then mtopp=mtop
						Next
						If ciatomtotciai1=0 Then
							ciatomtotciai1=ciatomtotciaimax
							ciatomtotcia1=ciatomtotciamax
						End If
						If ciatomtotciai2=0 Then
							ciatomtotciai2=ciatomtotciaimin
							ciatomtotcia2=ciatomtotciamin
						End If
						'this just shows a larger circle representing the start of the arc
						'Circle (multifxm(ciatomtotciai1),multifym(ciatomtotciai1)),18
						'Circle (multifxm(ciatomtotciai2),multifym(ciatomtotciai2)),8
						
						
						Select Case teotd
							Case "circle","arc"
								Circle (circles(tci,1),circles(tci,2)),circles(tci,4),10,ciatomtotcia1,ciatomtotcia2
							Case "ellipse","elliptical arc"

								'plot green elliptical arc
								'convert radians to degrees
								'using circle command temporarily - shoud be plot ellipse
								'Circle (circles(tci,1),circles(tci,2)),circles(tci,4),10,ciatomtotcia1,ciatomtotcia2
								'ciatomtotcia1=ciatomtotcia1*(180/PI)
								'ciatomtotcia2=ciatomtotcia2*(180/PI)
								plotellipse(circles(tci,1),circles(tci,2),0,circles(tci,4),10,ciatomtotcia1,ciatomtotcia2,circles(tci,8),circles(tci,11))
						End Select
						
						

						Select Case tesptype
							Case "circle","ellipse"
								tesp=1
								ipointsa(1)=ciatomtotcia2
								ipointsa(2)=ciatomtotcia1
							Case "arc","elliptical arc"
								tesp=2
								ipointsa(1)=circles(tci,6)'actual start
								ipointsa(2)=ciatomtotcia1 'start
								ipointsa(3)=ciatomtotcia2 'end
								ipointsa(4)=circles(tci,7)'actual end
								Select Case ciatomtotcia1
									Case circles(tci,6)-.0000000000001 To  circles(tci,6)+.0000000000001
										tesp=1
										ipointsa(1)=ciatomtotcia2 'end
										ipointsa(2)=circles(tci,7)'actual end
								End Select
								Select Case ciatomtotcia2
									Case circles(tci,7)-.0000000000001 To  circles(tci,7)+.0000000000001
										tesp=1
										ipointsa(1)=circles(tci,6)'actual start
										ipointsa(2)=ciatomtotcia1 'start
								End Select
						End Select
				End Select
			EndIf
			If buttonson(62)=TRUE Then'show extend
				Select Case teotd
					Case "line"
					Case "circle"
					Case "arc"
					Case "ellipse"
					Case "elliptical arc"
				End Select
			EndIf
		End If
	Else
		infobox(fbcad_desc)
		selline=0
		selcircle=0
		selentity=FALSE
		otd=""
		trackangle
		tracklength
	EndIf
End Sub
Sub trimobjects
	Select Case tesptype
		Case "line"
			modify=0
			lines(trimlinei,8)=-1
			redraw
			''showgroups
			Select Case tesp
				Case 1
					lc=lines(trimlinei,7)
					x1=ipointsx(1)
					y1=ipointsy(1)
					x2=ipointsx(2)
					y2=ipointsy(2)
					createline
					lines(linec,8)=1
				Case 2
					lc=lines(trimlinei,7)
					x1=ipointsx(1)
					y1=ipointsy(1)
					x2=ipointsx(2)
					y2=ipointsy(2)
					createline
					lines(linec,8)=1
					x1=ipointsx(3)
					y1=ipointsy(3)
					x2=ipointsx(4)
					y2=ipointsy(4)
					createline
					lines(linec,8)=1
			End Select
			multic=0
		Case "circle","arc"
			modify=0
			circles(tci,10)=-1
			redraw
			Select Case tesp
				Case 1
					x1=circles(tci,1)
					y1=circles(tci,2)
					radius=circles(tci,4)
					lc=circles(tci,5)
					arcstart=ipointsa(1)
					arcend=ipointsa(2)
					createarc
					circles(circlec,10)=1
				Case 2
					x1=circles(tci,1)
					y1=circles(tci,2)
					radius=circles(tci,4)
					lc=circles(tci,5)
					arcstart=ipointsa(1)
					arcend=ipointsa(2)
					createarc
					circles(circlec,10)=1
					
					x1=circles(tci,1)
					y1=circles(tci,2)
					radius=circles(tci,4)
					lc=circles(tci,5)
					arcstart=ipointsa(3)
					arcend=ipointsa(4)
					createarc
					circles(circlec,10)=1
			End Select
			multic=0
			''showgroups
		Case "ellipse","elliptical arc"
			modify=0
			circles(tci,10)=-1
			redraw
			Select Case tesp
				Case 1
					x1=circles(tci,1)
					y1=circles(tci,2)
					radius=circles(tci,4)
					lc=circles(tci,5)
					arcstart=ipointsa(1)
					arcend=ipointsa(2)
					eradius=circles(tci,8)
					erotation=circles(tci,11)
					createellipse
					circles(circlec,10)=1
				Case 2
					x1=circles(tci,1)
					y1=circles(tci,2)
					radius=circles(tci,4)
					lc=circles(tci,5)
					arcstart=ipointsa(1)
					arcend=ipointsa(2)
					eradius=circles(tci,8)
					erotation=circles(tci,11)
					createellipse
					circles(circlec,10)=1
					
					x1=circles(tci,1)
					y1=circles(tci,2)
					radius=circles(tci,4)
					lc=circles(tci,5)
					arcstart=ipointsa(3)
					arcend=ipointsa(4)
					eradius=circles(tci,8)
					erotation=circles(tci,11)
					createellipse
					circles(circlec,10)=1
			End Select
			multic=0
			''showgroups
	End Select
	'showgroups
	tesptype=""
	tesp=0
End Sub
Sub calcarcmidpoint()
	If circles(selcircle,6)>circles(selcircle,7) Then
		arcmidpointx=circles(selcircle,1)+cos((circles(selcircle,6)-circles(selcircle,7))/2+circles(selcircle,7)+PI)*circles(selcircle,4)
		arcmidpointy=circles(selcircle,2)+sin((circles(selcircle,6)-circles(selcircle,7))/2+circles(selcircle,7)+PI)*circles(selcircle,4)
	Else
		arcmidpointx=circles(selcircle,1)+cos(circles(selcircle,6)+(circles(selcircle,7)-circles(selcircle,6))/2)*circles(selcircle,4)
		arcmidpointy=circles(selcircle,2)+sin(circles(selcircle,6)+(circles(selcircle,7)-circles(selcircle,6))/2)*circles(selcircle,4)
	End If
End Sub
Sub calcarcmidpoint2(i As Integer)
	If circles(i,6)>circles(i,7) Then
		arcmidpointx=circles(i,1)+cos((circles(i,6)-circles(i,7))/2+circles(i,7)+PI)*circles(i,4)
		arcmidpointy=circles(i,2)+sin((circles(i,6)-circles(i,7))/2+circles(i,7)+PI)*circles(i,4)
	Else
		arcmidpointx=circles(i,1)+cos(circles(i,6)+(circles(i,7)-circles(i,6))/2)*circles(i,4)
		arcmidpointy=circles(i,2)+sin(circles(i,6)+(circles(i,7)-circles(i,6))/2)*circles(i,4)
	End If
End Sub
Sub calcarcendpoints()
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	arcendpoint1x=circles(selcircle,1)+cos(circles(selcircle,6))*circles(selcircle,4)
	arcendpoint1y=circles(selcircle,2)+sin(circles(selcircle,6))*circles(selcircle,4)
	arcendpoint2x=circles(selcircle,1)+cos(circles(selcircle,7))*circles(selcircle,4)
	arcendpoint2y=circles(selcircle,2)+sin(circles(selcircle,7))*circles(selcircle,4)
End Sub
Sub calcarcendpoints2(i As Integer)
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	arcendpoint1x=circles(i,1)+cos(circles(i,6))*circles(i,4)
	arcendpoint1y=circles(i,2)+sin(circles(i,6))*circles(i,4)
	arcendpoint2x=circles(i,1)+cos(circles(i,7))*circles(i,4)
	arcendpoint2y=circles(i,2)+sin(circles(i,7))*circles(i,4)
End Sub
Sub calcellipsarcendpoints(i As Integer)
	Dim As Double epx1,epy1,epx2,epy2,epx3,epy3,epx4,epy4
	Dim As Double easpect

	easpect=circles(i,4)/circles(i,8)
	
	epx1=circles(i,1)+(Cos(circles(i,6)*d2r)*circles(i,8)*easpect)
	epy1=circles(i,2)+(Sin(circles(i,6)*d2r)*circles(i,8))
	x1p=epx1-circles(i,1)
	y1p=epy1-circles(i,2)
	epx1=x1p*Cos(circles(i,11)*d2r) - y1p*Sin(circles(i,11)*d2r)+circles(i,1)
	epy1=y1p*Cos(circles(i,11)*d2r) + x1p*Sin(circles(i,11)*d2r)+circles(i,2)

	epx2=circles(i,1)+(Cos(circles(i,7)*d2r)*circles(i,8)*easpect)
	epy2=circles(i,2)+(Sin(circles(i,7)*d2r)*circles(i,8))
	x1p=epx2-circles(i,1)
	y1p=epy2-circles(i,2)
	epx2=x1p*Cos(circles(i,11)*d2r) - y1p*Sin(circles(i,11)*d2r)+circles(i,1)
	epy2=y1p*Cos(circles(i,11)*d2r) + x1p*Sin(circles(i,11)*d2r)+circles(i,2)

	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	arcendpoint1x=epx1
	arcendpoint1y=epy1
	arcendpoint2x=epx2
	arcendpoint2y=epy2
End Sub
Sub calcellipsarcmidpoint(i As Integer)

	Dim As Double epx1,epy1,midangle
	Dim As Double easpect
	
	midangle=circles(i,6)+((circles(i,7)-circles(i,6))/2)
	If circles(i,6)>circles(i,7) Then midangle=midangle+180
	
	easpect=circles(i,4)/circles(i,8)
	
	epx1=circles(i,1)+(Cos(midangle*d2r)*circles(i,8)*easpect)
	epy1=circles(i,2)+(Sin(midangle*d2r)*circles(i,8))
	x1p=epx1-circles(i,1)
	y1p=epy1-circles(i,2)
	epx1=x1p*Cos(circles(i,11)*d2r) - y1p*Sin(circles(i,11)*d2r)+circles(i,1)
	epy1=y1p*Cos(circles(i,11)*d2r) + x1p*Sin(circles(i,11)*d2r)+circles(i,2)

	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	arcmidpointx=epx1
	arcmidpointy=epy1
End Sub
sub hilisnap()
	Dim As Integer i
	ScreenSet 2,2
	for i = 11 to 20
		if buttonson(i)=true then
			select case i
				case 11
					Line (fxm-detect,fym-detect)-(fxm+detect,fym+detect),14,b
					exit for
				case 12
					calcmidpoint
					circle (fxm,fym),detect,14
					exit For
				case 13
					calcperpendicular
					circle (fxm,fym),detect,14
					exit For
				Case 14,15
					'for some reason i don't use hilisnap
					'14 & 15 are tangengt functions
				Case 18
					Select Case intersection
						Case "inside"
							circle (fxm,fym),detect,14
							exit For
						Case "outside"
							'Print intersection'outside
						Case else
							'Print intersection'parallel
					End Select
				Case 19
					calcnearestpoint
					circle (fxm,fym),detect,14
					exit For
			End select
		end if
	next
End Sub
Sub findsecondline()
	Dim As Integer i
	dp=false
	for dpmy=scrnmy-10 to scrnmy+10
		pp=point(dpmx,dpmy)
		PSet (dpmx,dpmy),31
		Select case pp
			Case 0,31',-1
			case Else
				dp=TRUE
				Exit For
		End Select
	Next
	If dp=FALSE Then
		dpmy=scrnmy
		For dpmx=scrnmx-10 To scrnmx+10
			pp=point(dpmx,dpmy)
			Select case pp
				Case 0,31',-1
				case Else
					dp=TRUE
					Exit For

			End Select
		Next
	End If
	If dp=TRUE Then
		for i = linec to 1 step -1
			if lines(i,8)>-1 And lines(i,7)=pp And i <> aintersect Then
				'window (wx1,wy1)-(wx2,wy2)
				line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),31
				'window
				if point(dpmx,dpmy)=31 Then
					bintersect=i
					exit for
				end If
			End if
		Next
	End if
End sub
sub group()'this is used to select / deselect single entities for a group
	Dim As Integer i,j,k,blocknumber,toplevelblocknumber
	
	If selline=0 Then'meaning a circle might be selected instead of a line
		tempint=circles(selcircle,10)
		Select Case circles(selcircle,10)'what is the state of the selected circle
			Case 0'if it's not part of any group then
				circles(selcircle,10)=1'set this circle to group 1
				'if the circle is part of a block then select it (the block) too
				If circles(selcircle,12)>0 Then
					'For i = 1 To linec 
					'	If lines(i,9)=circles(selcircle,12) Then lines(i,8)=1
					'Next
					blocknumber=circles(selcircle,12)
					If blocklevelc>0 Then
						If blocklevel(blocknumber,1)=0 Then
							toplevelblocknumber=blocknumber
						Else
							toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
						EndIf
					Else
						toplevelblocknumber=blocknumber
					End If
					
					for i = 1 To linec
						if lines(i,9)=blocknumber Or lines(i,9)=toplevelblocknumber Then
							lines(i,8)=1
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If lines(i,9)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(lines(i,9),k+1)=toplevelblocknumber Then
													lines(i,8)=1
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
					'For i = 1 To circlec 
					'	If circles(i,12)=circles(selcircle,12) Then circles(i,10)=1
					'Next
					for i = 1 To circlec
						if circles(i,12)=blocknumber Or circles(i,12)=toplevelblocknumber Then
							circles(i,10)=1
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If circles(i,12)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(circles(i,12),k+1)=toplevelblocknumber Then
													circles(i,10)=1
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
				EndIf
			Case 1'if it's part of group 1 then
				circles(selcircle,10)=0'set this circle to no group
				'If the circle is part of a block then deselect it (the block) too
				If circles(selcircle,12)>0 Then
					'For i = 1 To linec 
					'	If lines(i,9)=circles(selcircle,12) Then lines(i,8)=0
					'Next
					'For i = 1 To circlec 
					'	If circles(i,12)=circles(selcircle,12) Then circles(i,10)=0
					'Next
					blocknumber=circles(selcircle,12)
					If blocklevelc>0 Then
						If blocklevel(blocknumber,1)=0 Then
							toplevelblocknumber=blocknumber
						Else
							toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
						EndIf
					Else
						toplevelblocknumber=blocknumber
					End If
					
					for i = 1 To linec
						if lines(i,9)=blocknumber Or lines(i,9)=toplevelblocknumber Then
							lines(i,8)=0
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If lines(i,9)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(lines(i,9),k+1)=toplevelblocknumber Then
													lines(i,8)=0
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
					'For i = 1 To circlec 
					'	If circles(i,12)=circles(selcircle,12) Then circles(i,10)=1
					'Next
					for i = 1 To circlec
						if circles(i,12)=blocknumber Or circles(i,12)=toplevelblocknumber Then
							circles(i,10)=0
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If circles(i,12)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(circles(i,12),k+1)=toplevelblocknumber Then
													circles(i,10)=0
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
				EndIf
			'Case Is > 1 'not sure why it would be >1
			'	For i = 1 To linec 
			'		If lines(i,8)=tempint Then
			'			lines(i,8)=1
			'		EndIf
			'	Next
			'	For i = 1 To circlec 
			'		If circles(i,10)=tempint Then
			'			circles(i,10)=1
			'		EndIf
			'	Next
		End Select
	End If
	If selcircle=0 Then
		tempint=lines(selline,8)
		Select Case lines(selline,8)
			Case 0'select the line
				lines(selline,8)=1
				'if the line is part of a block then select it (the block) too
				If lines(selline,9)>0 Then
					'For i = 1 To linec 
					'	If lines(i,9)=lines(selline,9) Then lines(i,8)=1
					'Next
					'For i = 1 To circlec 
					'	If circles(i,12)=lines(selline,9) Then circles(i,10)=1
					'Next
					blocknumber=lines(selline,9)
					If blocklevelc>0 Then
						If blocklevel(blocknumber,1)=0 Then
							toplevelblocknumber=blocknumber
						Else
							toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
						EndIf
					Else
						toplevelblocknumber=blocknumber
					End If
					theboxbelow("Block selected "+blocknames(toplevelblocknumber)+" "+Str(toplevelblocknumber))
					For i = 1 To linec
						if lines(i,9)=blocknumber Or lines(i,9)=toplevelblocknumber Then
							lines(i,8)=1
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If lines(i,9)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(lines(i,9),k+1)=toplevelblocknumber Then
													lines(i,8)=1
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
					'For i = 1 To circlec 
					'	If circles(i,12)=circles(selcircle,12) Then circles(i,10)=1
					'Next
					for i = 1 To circlec
						If circles(i,12)=blocknumber Or circles(i,12)=toplevelblocknumber Then
							circles(i,10)=1
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If circles(i,12)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(circles(i,12),k+1)=toplevelblocknumber Then
													circles(i,10)=1
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
				EndIf
			Case 1'deselect the line
				lines(selline,8)=0
				'if the line is part of a block then deselect it (the block) too
				If lines(selline,9)>0 Then
					'For i = 1 To linec 
					'	If lines(i,9)=lines(selline,9) Then lines(i,8)=0
					'Next
					'For i = 1 To circlec 
					'	If circles(i,12)=lines(selline,9) Then circles(i,10)=0
					'Next
					blocknumber=lines(selline,9)
					If blocklevelc>0 Then
						If blocklevel(blocknumber,1)=0 Then
							toplevelblocknumber=blocknumber
						Else
							toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
						EndIf
					Else
						toplevelblocknumber=blocknumber
					End If
					theboxbelow("Block deselected "+blocknames(toplevelblocknumber))
					for i = 1 To linec
						if lines(i,9)=blocknumber Or lines(i,9)=toplevelblocknumber Then
							lines(i,8)=0
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If lines(i,9)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(lines(i,9),k+1)=toplevelblocknumber Then
													lines(i,8)=0
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next
					'For i = 1 To circlec 
					'	If circles(i,12)=circles(selcircle,12) Then circles(i,10)=1
					'Next
					for i = 1 To circlec
						if circles(i,12)=blocknumber Or circles(i,12)=toplevelblocknumber Then
							circles(i,10)=0
						Else
							If blocklevelc>0 Then
								'is this a top level block or is it one in the nest
								If circles(i,12)>0 Then
									For j=1 To blockc
										If j<>blocknumber Then
											For k=1 To blocklevelc
												If blocklevel(circles(i,12),k+1)=toplevelblocknumber Then
													circles(i,10)=0
													Exit For
												EndIf
											Next
										End If
									Next
								End If
							End If
						end if
					Next

				EndIf
			'Case Is > 1'i think a line would be >1 if it was part of a group and if so then i just set it to 1 (selected)
			'	For i = 1 To linec 
			'		If lines(i,8)=tempint Then
			'			lines(i,8)=1
			'		EndIf
			'	Next
			'	For i = 1 To circlec 
			'		If circles(i,10)=tempint Then
			'			circles(i,10)=1
			'		EndIf
			'	Next
		End Select
	End If
	redraw
End Sub
Sub groupbox(g As Integer, s As integer)
	'groupbox(0,1)'left mouse button
	'groupbox(1,0)'right mouse button
	'exit sub
	'curious to know if setting group code for selected entities to
	'block number instead of 1 is used for any other routines
	'the only place the higher value is evaluated is in this sub routine
	'ie. in the If lines(i,8)=g Then found below
	'other routines sometimes chech if lines(i,8)=> 1 but not if > 1 then do this instead
	'even stranger is that when calling this routine g is only 0 or 1
	'it was probably an idea that was never used
	Dim As Integer i,j,k,blocknumber,toplevelblocknumber
	Dim As Integer bibi
	Dim As BOOLEAN blocksinbox(blockc)
	For i = 1 To blockc
		blocksinbox(i)=FALSE
	Next
	If boxselectx1>boxselectx2 Then swap boxselectx1,boxselectx2
	If boxselecty1>boxselecty2 Then swap boxselecty1,boxselecty2
	For i = 1 To linec
		If shift_key=1 Or shift_key=2 Then'box selecting blocks
			If lines(i,8)>-1 Then
				If layerstate(lines(i,7))=2 Then
					Select case lines(i,1)
						Case boxselectx1 To boxselectx2
							Select Case lines(i,2)
								Case boxselecty1 To boxselecty2
									Select case lines(i,4)
										Case boxselectx1 To boxselectx2
											Select Case lines(i,5)
												Case boxselecty1 To boxselecty2
													lines(i,8)=s
													'If s=0 Then
													'	If lines(i,9)>0 Then lines(i,8)=1'lines(i,9)
													'EndIf
											End Select
									End Select
							End Select
					End Select
				End If
			End if
		Else
			If lines(i,8)=g Then
				If layerstate(lines(i,7))=2 Then
					Select case lines(i,1)
						Case boxselectx1 To boxselectx2
							Select Case lines(i,2)
								Case boxselecty1 To boxselecty2
									Select case lines(i,4)
										Case boxselectx1 To boxselectx2
											Select Case lines(i,5)
												Case boxselecty1 To boxselecty2
													If lines(i,9)=0 Then
														lines(i,8)=s
													Else
														'select or deselect the block
														blocksinbox(lines(i,9))=true
													EndIf
	'												If s=0 Then
	'													If lines(i,9)>0 Then lines(i,8)=lines(i,9)
	'												EndIf
											End Select
									End Select
							End Select
					End Select
				End If
			End if
		EndIf

	Next
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	For i = 1 To circlec
		If shift_key=1 Or shift_key=2 Then
			If circles(i,10)>-1 Then
				If layerstate(circles(i,5))=2 Then
					Select Case circles(i,9)
						Case 1'circles
							Select Case circles(i,1)-circles(i,4)
								Case boxselectx1 To boxselectx2
									Select Case circles(i,1)+circles(i,4)
										Case boxselectx1 To boxselectx2
											Select Case circles(i,2)-circles(i,4)
												Case boxselecty1 To boxselecty2
													Select Case circles(i,2)+circles(i,4)
														Case boxselecty1 To boxselecty2
															'circle is in the box
															circles(i,10)=s
															'If s=0 Then
															'	If circles(i,12)>0 Then circles(i,10)=1'circles(i,12)
															'EndIf
													End Select
											End Select
									End Select
							End Select
						Case 2 To 4
							If circles(i,9) = 2 Then
								boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6)*180/pi,circles(i,7)*180/pi,circles(i,4),circles(i,11))
							Else
								boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
							EndIf
							Select Case boxarcx1
								Case boxselectx1 To boxselectx2
									Select Case boxarcx2
										Case boxselectx1 To boxselectx2
											Select Case boxarcy1
												Case boxselecty1 To boxselecty2
													Select Case boxarcy2
														Case boxselecty1 To boxselecty2
															'ellipse is in the box
															circles(i,10)=s
															'If s=0 Then
															'	If circles(i,12)>0 Then circles(i,10)=1'circles(i,12)
															'EndIf
													End Select
											End Select
									End Select
							End Select
					End Select
				End If
			End if
		Else
			If circles(i,10)=g Then
				If layerstate(circles(i,5))=2 Then
					Select Case circles(i,9)
						Case 1'circles
							Select Case circles(i,1)-circles(i,4)
								Case boxselectx1 To boxselectx2
									Select Case circles(i,1)+circles(i,4)
										Case boxselectx1 To boxselectx2
											Select Case circles(i,2)-circles(i,4)
												Case boxselecty1 To boxselecty2
													Select Case circles(i,2)+circles(i,4)
														Case boxselecty1 To boxselecty2
															'circle is in the box
															If circles(i,12)=0 Then
																circles(i,10)=s
															Else
																blocksinbox(circles(i,12))=TRUE
															EndIf
													End Select
											End Select
									End Select
							End Select
						Case 2 To 4
							If circles(i,9) = 2 Then
								boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6)*180/pi,circles(i,7)*180/pi,circles(i,4),circles(i,11))
							Else
								boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
							EndIf
							Select Case boxarcx1
								Case boxselectx1 To boxselectx2
									Select Case boxarcx2
										Case boxselectx1 To boxselectx2
											Select Case boxarcy1
												Case boxselecty1 To boxselecty2
													Select Case boxarcy2
														Case boxselecty1 To boxselecty2
															'ellipse is in the box
															If circles(i,12)=0 Then
																circles(i,10)=s
															Else
																blocksinbox(circles(i,12))=TRUE
															EndIf
													End Select
											End Select
									End Select
							End Select
					End Select
				End If
			End if
		EndIf
	Next
	If shift_key=0 Then
		If blocklevelc>0 Then
			For bibi=1 To blockc
				If blocksinbox(bibi)=TRUE Then
					blocknumber=bibi
					If blocklevel(blocknumber,1)=0 Then
						toplevelblocknumber=blocknumber
						blocksinbox(blocknumber)=TRUE
					Else
						toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
						blocksinbox(toplevelblocknumber)=TRUE
					EndIf
					For i=1 To blockc
						If blocklevel(i,blocklevel(i,1)+1)=toplevelblocknumber Then
							blocksinbox(i)=TRUE
						EndIf
					Next
				EndIf
			Next
		End If
		For i = 1 To linec
			If lines(i,9)>0 And lines(i,8)<>-1 Then
				If blocksinbox(lines(i,9))=TRUE Then lines(i,8)=s
			End If
		Next
		For i = 1 To circlec
			If circles(i,12)>0 And circles(i,10)<>-1 Then
				If blocksinbox(circles(i,12))=TRUE Then circles(i,10)=s
			End If
		Next
	End If
	'screenset 1,1
	'view (drawareax1,drawareay1)-(drawareax2,drawareay2)
	'Window (wx1,wy1)-(wx2,wy2)
	redraw
	'showgroups
End Sub
Sub hiliblock(blocknumber As Integer)
	ScreenSet 2,2
	Dim As Integer i,j,k,toplevelblocknumber
	If blocklevelc>0 Then
		If blocklevel(blocknumber,1)=0 Then
			toplevelblocknumber=blocknumber
		Else
			toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
		EndIf
	Else
		toplevelblocknumber=blocknumber
	End If
	infobox("Block " + blocknames(toplevelblocknumber))
	for i = 1 To linec
		if lines(i,9)=blocknumber Or lines(i,9)=toplevelblocknumber Then
			line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),15
		Else
			If blocklevelc>0 Then
				'is this a top level block or is it one in the nest
				If lines(i,9)>0 Then
					For j=1 To blockc
						If j<>blocknumber Then
							For k=1 To blocklevelc
								If blocklevel(lines(i,9),k+1)=toplevelblocknumber Then
									line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),15
									Exit For
								EndIf
							Next
						End If
					Next
				End If
			End If
		end if
	Next
	For i = 1 To circlec
		if circles(i,12)=blocknumber Or circles(i,12)=toplevelblocknumber Then
			Select Case circles(i,9)
				Case 1
					circle(circles(i,1),circles(i,2)),circles(i,4),15
				Case 2
					circle(circles(i,1),circles(i,2)),circles(i,4),15,circles(i,6),circles(i,7)
				Case 3,4
					plotellipse(circles(i,1),circles(i,2),circles(i,3),circles(i,4),15,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
			End Select
		Else
			If blocklevelc>0 Then
				'is this a top level block or is it one in the nest
				If circles(i,12)>0 Then
					For j=1 To blockc
						If j<>blocknumber Then
							For k=1 To blocklevelc
								If blocklevel(circles(i,12),k+1)=toplevelblocknumber Then
									Select Case circles(i,9)
										Case 1
											circle(circles(i,1),circles(i,2)),circles(i,4),15
										Case 2
											circle(circles(i,1),circles(i,2)),circles(i,4),15,circles(i,6),circles(i,7)
										Case 3,4
											plotellipse(circles(i,1),circles(i,2),circles(i,3),circles(i,4),15,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
									End Select
									Exit For
								EndIf
							Next
						End If
					Next
				End If
			End If
		end if
	next
End Sub
Sub findallselectedentities()
	selectedentitiestotal=0
	selectedentitieslinecount=0
	selectedentitiescirclecount=0
	selectedentitiesexists=FALSE
	Dim As Integer i
	for i = preloadedlinec+1 To linec
		If lines(i,8)=1 Then
			selectedentitiestotal=selectedentitiestotal+1
			selectedentitieslinecount=selectedentitieslinecount+1
		end if
	Next
	For i = preloadedcirclec+1 To circlec
		If circles(i,10)=1 Then
			selectedentitiestotal=selectedentitiestotal+1
			selectedentitiescirclecount=selectedentitiescirclecount+1
		EndIf
	Next
	If selectedentitiestotal<>0 Then selectedentitiesexists=TRUE
End Sub
Sub buildselectedentityarrays()'do this if selectedentitiesexists=TRUE
	ReDim selectedentitylines(selectedentitieslinecount)
	ReDim selectedentitycircles(selectedentitiescirclecount)
	selectedentitiestotal=0
	selectedentitieslinecount=0
	selectedentitiescirclecount=0
	selectedentitiesexists=FALSE
	Dim As Integer i
	for i = preloadedlinec+1 To linec
		If lines(i,8)=1 Then
			selectedentitiestotal=selectedentitiestotal+1
			selectedentitieslinecount=selectedentitieslinecount+1
			selectedentitylines(selectedentitieslinecount)=i
		end if
	Next
	For i = preloadedcirclec+1 To circlec
		If circles(i,10)=1 Then
			selectedentitiestotal=selectedentitiestotal+1
			selectedentitiescirclecount=selectedentitiescirclecount+1
			selectedentitycircles(selectedentitiescirclecount)=i
		EndIf
	Next
	If selectedentitiestotal<>0 Then selectedentitiesexists=TRUE
	theboxbelow(Str(selectedentitiescirclecount))
End Sub
Sub parallelcontour()
	Dim As Integer i,k
	Dim As BOOLEAN contourfound,contourerror
	Dim As Double temparcendpoint1x,temparcendpoint1y,temparcendpoint2x,temparcendpoint2y
	contourfound=FALSE
	contourerror=FALSE
	entitycircuit="open"
	findallselectedentities
	If selectedentitiesexists=TRUE Then
		buildselectedentityarrays
		ReDim joinedentityorder(selectedentitiestotal,6)
		If selline<>0 Then
			joinedentityorder(1,1)=1'line
			For k=1 To selectedentitieslinecount
				If selectedentitylines(k)=selline Then
					joinedentityorder(1,2)=selectedentitylines(k)
					If lines(joinedentityorder(1,2),1)=fxm And lines(joinedentityorder(1,2),2)=fym Then
						joinedentityorder(1,3)=1
					Else
						joinedentityorder(1,3)=2
					EndIf
					Exit For
				EndIf
			Next
		Else
			joinedentityorder(1,1)=2'arc
			For k=1 To selectedentitiescirclecount
				If selectedentitycircles(k)=selcircle Then
					joinedentityorder(1,2)=selectedentitycircles(k)
					calcarcendpoints2(joinedentityorder(1,2))
					If arcendpoint1x=fxm And arcendpoint1y=fym Then
						joinedentityorder(1,3)=1'arcstart
					Else
						joinedentityorder(1,3)=2'arcend
					EndIf
					Exit For
				EndIf
			Next
		EndIf
		For i = 1 To selectedentitiestotal-1
			Select Case joinedentityorder(i,1)
				Case 1'is this line connected to another line?
					For k=1 To selectedentitieslinecount
						If selectedentitylines(k)<>joinedentityorder(i,2) Then
							Select Case joinedentityorder(i,3)
								Case 1
									If calcd(_
										lines(joinedentityorder(i,2),4),_
										lines(joinedentityorder(i,2),5),_
										0,_
										lines(selectedentitylines(k),1),_
										lines(selectedentitylines(k),2),_
										0_
											) < jointolerance Then
										joinedentityorder(i+1,1)=1'line
										joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
										joinedentityorder(i+1,3)=1'the x1,y1 of this other line
										Exit For
									Else
										If calcd(_
											lines(joinedentityorder(i,2),4),_
											lines(joinedentityorder(i,2),5),_
											0,_
											lines(selectedentitylines(k),4),_
											lines(selectedentitylines(k),5),_
											0_
												) < jointolerance Then
											joinedentityorder(i+1,1)=1'line
											joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
											joinedentityorder(i+1,3)=2'the x2,y2 of this other line
											Exit For
										End If
									End If
								Case 2
									If calcd(_
										lines(joinedentityorder(i,2),1),_
										lines(joinedentityorder(i,2),2),_
										0,_
										lines(selectedentitylines(k),1),_
										lines(selectedentitylines(k),2),_
										0_
											) < jointolerance Then
										joinedentityorder(i+1,1)=1'line
										joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
										joinedentityorder(i+1,3)=1'the x1,y1 of this other line
										Exit For
									Else
										If calcd(_
											lines(joinedentityorder(i,2),1),_
											lines(joinedentityorder(i,2),2),_
											0,_
											lines(selectedentitylines(k),4),_
											lines(selectedentitylines(k),5),_
											0_
												) < jointolerance Then
											joinedentityorder(i+1,1)=1'line
											joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
											joinedentityorder(i+1,3)=2'the x2,y2 of this other line
											Exit For
										End If
									End If
							End Select
						EndIf
					Next
					'or arcs assuming its an arc? and not a circle or ellipse or elliptical arc
					If joinedentityorder(i+1,1)=0 Then
						For k=1 To selectedentitiescirclecount
							calcarcendpoints2(selectedentitycircles(k))
							Select Case joinedentityorder(i,3)
								Case 1
									If calcd(_
										lines(joinedentityorder(i,2),4),_
										lines(joinedentityorder(i,2),5),_
										0,_
										arcendpoint1x,_
										arcendpoint1y,_
										0_
											) < jointolerance Then
										joinedentityorder(i+1,1)=2'arc
										joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
										joinedentityorder(i+1,3)=1'arcstart of this arc
										Exit For
									Else
										If calcd(_
											lines(joinedentityorder(i,2),4),_
											lines(joinedentityorder(i,2),5),_
											0,_
											arcendpoint2x,_
											arcendpoint2y,_
											0_
												) < jointolerance Then
											joinedentityorder(i+1,1)=2'arc
											joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
											joinedentityorder(i+1,3)=2'arcend of this arc
											Exit For
										End If
									End If
								Case 2
									If calcd(_
										lines(joinedentityorder(i,2),1),_
										lines(joinedentityorder(i,2),2),_
										0,_
										arcendpoint1x,_
										arcendpoint1y,_
										0_
											) < jointolerance Then
										joinedentityorder(i+1,1)=2'arc
										joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
										joinedentityorder(i+1,3)=1'arcstart of this arc
										Exit For
									Else
										If calcd(_
											lines(joinedentityorder(i,2),1),_
											lines(joinedentityorder(i,2),2),_
											0,_
											arcendpoint2x,_
											arcendpoint2y,_
											0_
												) < jointolerance Then
											joinedentityorder(i+1,1)=2'arc
											joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
											joinedentityorder(i+1,3)=2'arcend of this arc
											Exit For
										End If
									End If
							End Select
						Next
					EndIf
				Case 2'is this arc connected to
					'a line
					'but first - get this arc's arcstart/arcend xy points
					calcarcendpoints2(joinedentityorder(i,2))
					temparcendpoint1x=arcendpoint1x
					temparcendpoint1y=arcendpoint1y
					temparcendpoint2x=arcendpoint2x
					temparcendpoint2y=arcendpoint2y
					For k=1 To selectedentitieslinecount
						Select Case joinedentityorder(i,3)
							Case 1'arcstart
								If calcd(_
									temparcendpoint2x,_
									temparcendpoint2y,_
									0,_
									lines(selectedentitylines(k),1),_
									lines(selectedentitylines(k),2),_
									0_
										) < jointolerance Then
									joinedentityorder(i+1,1)=1'line
									joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
									joinedentityorder(i+1,3)=1'the x1,y1 of this other line
									Exit For
								Else
									If calcd(_
										temparcendpoint2x,_
										temparcendpoint2y,_
										0,_
										lines(selectedentitylines(k),4),_
										lines(selectedentitylines(k),5),_
										0_
											) < jointolerance Then
										joinedentityorder(i+1,1)=1'line
										joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
										joinedentityorder(i+1,3)=2'the x2,y2 of this other line
										Exit For
									End If
								End If
							Case 2'arcend
								If calcd(_
									temparcendpoint1x,_
									temparcendpoint1y,_
									0,_
									lines(selectedentitylines(k),1),_
									lines(selectedentitylines(k),2),_
									0_
										) < jointolerance Then
									joinedentityorder(i+1,1)=1'line
									joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
									joinedentityorder(i+1,3)=1'the x1,y1 of this other line
									Exit For
								Else
									If calcd(_
										temparcendpoint1x,_
										temparcendpoint1y,_
										0,_
										lines(selectedentitylines(k),4),_
										lines(selectedentitylines(k),5),_
										0_
											) < jointolerance Then
										joinedentityorder(i+1,1)=1'line
										joinedentityorder(i+1,2)=selectedentitylines(k)'this other line
										joinedentityorder(i+1,3)=2'the x2,y2 of this other line
										Exit For
									End If
								End If
						End Select
					Next
					'or another arc
					If joinedentityorder(i+1,1)=0 Then
						For k=1 To selectedentitiescirclecount
							If selectedentitycircles(k)<>joinedentityorder(i,2) Then
								calcarcendpoints2(selectedentitycircles(k))
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											temparcendpoint2x,_
											temparcendpoint2y,_
											0,_
											arcendpoint1x,_
											arcendpoint1y,_
											0_
												) < jointolerance Then
											joinedentityorder(i+1,1)=2'arc
											joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
											joinedentityorder(i+1,3)=1'arcstart of this arc
											Exit For
										Else
											If calcd(_
												temparcendpoint2x,_
												temparcendpoint2y,_
												0,_
												arcendpoint2x,_
												arcendpoint2y,_
												0_
													) < jointolerance Then
												joinedentityorder(i+1,1)=2'arc
												joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
												joinedentityorder(i+1,3)=2'arcend of this arc
												Exit For
											End If
										End If
									Case 2
										If calcd(_
											temparcendpoint1x,_
											temparcendpoint1y,_
											0,_
											arcendpoint1x,_
											arcendpoint1y,_
											0_
												) < jointolerance Then
											joinedentityorder(i+1,1)=2'arc
											joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
											joinedentityorder(i+1,3)=1'arcstart of this arc
											Exit For
										Else
											If calcd(_
												temparcendpoint1x,_
												temparcendpoint1y,_
												0,_
												arcendpoint2x,_
												arcendpoint2y,_
												0_
													) < jointolerance Then
												joinedentityorder(i+1,1)=2'arc
												joinedentityorder(i+1,2)=selectedentitycircles(k)'this arc
												joinedentityorder(i+1,3)=2'arcend of this arc
												Exit For
											End If
										End If
								End Select
							End If
						Next
					EndIf
			End Select
			If joinedentityorder(i+1,1)=0 Then
				'an error - one of the entities is not connected to others
				'maybe abort and indicate how many it did find connected
				'or maybe only proceed with those it did find connected
				contourerror=TRUE
				Exit For
			EndIf
			contourfound=TRUE
		Next
	EndIf
	If contourfound=TRUE And contourerror=FALSE Then
		'is the circuit open or closed
		
		i=selectedentitiestotal
		Select Case joinedentityorder(1,1)
			Case 1'first entity is a line
				Select Case joinedentityorder(i,1)
					Case 1'last entity is a line
						'are the end points of these lines joined
						Select Case joinedentityorder(1,3)
							Case 1
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											lines(joinedentityorder(1,2),1),_
											lines(joinedentityorder(1,2),2),_
											0,_
											lines(joinedentityorder(i,2),4),_
											lines(joinedentityorder(i,2),5),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											lines(joinedentityorder(1,2),1),_
											lines(joinedentityorder(1,2),2),_
											0,_
											lines(joinedentityorder(i,2),1),_
											lines(joinedentityorder(i,2),2),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
							Case 2
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											lines(joinedentityorder(1,2),4),_
											lines(joinedentityorder(1,2),5),_
											0,_
											lines(joinedentityorder(i,2),4),_
											lines(joinedentityorder(i,2),5),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											lines(joinedentityorder(1,2),4),_
											lines(joinedentityorder(1,2),5),_
											0,_
											lines(joinedentityorder(i,2),1),_
											lines(joinedentityorder(i,2),2),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
						End Select
					Case 2'last entity is an arc
						calcarcendpoints2(joinedentityorder(i,2))
						'is the line joined to the arc
						Select Case joinedentityorder(1,3)
							Case 1
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											lines(joinedentityorder(1,2),1),_
											lines(joinedentityorder(1,2),2),_
											0,_
											arcendpoint2x,_
											arcendpoint2y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											lines(joinedentityorder(1,2),1),_
											lines(joinedentityorder(1,2),2),_
											0,_
											arcendpoint1x,_
											arcendpoint1y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
							Case 2
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											lines(joinedentityorder(1,2),4),_
											lines(joinedentityorder(1,2),5),_
											0,_
											arcendpoint2x,_
											arcendpoint2y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											lines(joinedentityorder(1,2),4),_
											lines(joinedentityorder(1,2),5),_
											0,_
											arcendpoint1x,_
											arcendpoint1y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
						End Select
				End Select
			Case 2'first entity is an arc
				calcarcendpoints2(joinedentityorder(1,2))
				temparcendpoint1x=arcendpoint1x
				temparcendpoint1y=arcendpoint1y
				temparcendpoint2x=arcendpoint2x
				temparcendpoint2y=arcendpoint2y
				Select Case joinedentityorder(i,1)
					Case 1'last entity is a line
						'is the arc joined to this line
						Select Case joinedentityorder(1,3)
							Case 1
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											temparcendpoint1x,_
											temparcendpoint1y,_
											0,_
											lines(joinedentityorder(i,2),4),_
											lines(joinedentityorder(i,2),5),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											temparcendpoint1x,_
											temparcendpoint1y,_
											0,_
											lines(joinedentityorder(i,2),1),_
											lines(joinedentityorder(i,2),2),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
							Case 2
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											temparcendpoint2x,_
											temparcendpoint2y,_
											0,_
											lines(joinedentityorder(i,2),4),_
											lines(joinedentityorder(i,2),5),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											temparcendpoint2x,_
											temparcendpoint2y,_
											0,_
											lines(joinedentityorder(i,2),1),_
											lines(joinedentityorder(i,2),2),_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
						End Select
					Case 2'last entity is an arc
						calcarcendpoints2(joinedentityorder(i,2))
						'is the line joined to the arc
						Select Case joinedentityorder(1,3)
							Case 1
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											temparcendpoint1x,_
											temparcendpoint1y,_
											0,_
											arcendpoint2x,_
											arcendpoint2y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											temparcendpoint1x,_
											temparcendpoint1y,_
											0,_
											arcendpoint1x,_
											arcendpoint1y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
							Case 2
								Select Case joinedentityorder(i,3)
									Case 1
										If calcd(_
											temparcendpoint2x,_
											temparcendpoint2y,_
											0,_
											arcendpoint2x,_
											arcendpoint2y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
									Case 2
										If calcd(_
											temparcendpoint2x,_
											temparcendpoint2y,_
											0,_
											arcendpoint1x,_
											arcendpoint1y,_
											0_
												) < jointolerance Then
											entitycircuit="closed"
										End If
								End Select
						End Select
				End Select
		End Select
	EndIf
End Sub
Sub showgroups()
	'why not incorporate this as part of redraw
	'or better yet - since it only applies to entities inview
	'why not incorporate it in inview
	detect=(wx2-wx1)*detectsize
	detectingpoints=0
	'screenset 1,1:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	Dim As Integer i,ivi,selectcolor
	groupexists=FALSE
	'linesingroupc,circlesingroupc,singlelinen,singlecirclen
	linesingroupc=0
	circlesingroupc=0
	singlelinen=0
	firstlineingroup=0
	secondlineingroup=0
	singlecirclen=0
	firstcircleingroup=0
	secondcircleingroup=0
	for ivi = inviewlinesc to 1 step -1
		i=inviewlines(ivi)
		if lines(i,8)=1 Then
			groupexists=TRUE
			linesingroupc=linesingroupc+1
			singlelinen=i
			If firstlineingroup=0 Then
				firstlineingroup=i
			Else
				If secondlineingroup=0 Then secondlineingroup=i
			EndIf
			line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),35
			If buttonson(109)=TRUE Then
				'find extents
			Else
				If lines(i,9)>0 Then selectcolor=10 Else selectcolor=11
				line (lines(i,1)-detect,lines(i,2)-detect)-(lines(i,1)+detect,lines(i,2)+detect),selectcolor,b
				line (lines(i,4)-detect,lines(i,5)-detect)-(lines(i,4)+detect,lines(i,5)+detect),selectcolor,b
			End If
		Else
			'If lines(i,8)>=0 Then Line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),lines(i,7)
		end if
	Next
	'For i = 1 To circlec
	for ivi = inviewcirclesc to 1 step -1
		i=inviewcircles(ivi)
		If circles(i,10)=1 Then
			groupexists=TRUE
			circlesingroupc=circlesingroupc+1
			singlecirclen=i
			If firstcircleingroup=0 Then
				firstcircleingroup=i
			Else
				If secondcircleingroup=0 Then secondcircleingroup=i
			EndIf
			If buttonson(109)=TRUE Then
				'find extents
			Else
				If circles(i,12)>0 Then selectcolor=10 Else selectcolor=11
				circle (circles(i,1),circles(i,2)),detect,selectcolor
			End If
			Select Case circles(i,9)
				Case 1
					circle(circles(i,1),circles(i,2)),circles(i,4),35
				Case 2
					circle(circles(i,1),circles(i,2)),circles(i,4),35,circles(i,6),circles(i,7)
				Case 3,4
					plotellipse(circles(i,1),circles(i,2),circles(i,3),circles(i,4),35,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
			End Select
		Else
'			If circles(i,10)>=0 then
'				Select Case circles(i,9)
'					Case 1
'						circle(circles(i,1),circles(i,2)),circles(i,4),circles(i,5)
'					Case 2
'						circle(circles(i,1),circles(i,2)),circles(i,4),circles(i,5),circles(i,6),circles(i,7)
'					Case 3,4
'						plotellipse(circles(i,1),circles(i,2),circles(i,3),circles(i,4),Int(circles(i,5)),circles(i,6),circles(i,7),circles(i,8),circles(i,11))
'				End Select
'			End if
		EndIf
	Next

'Dim Shared As Integer grouplinec, groupcirclec
'Dim Shared As Integer grouplines(), groupcircles()
'Dim Shared As Byte groupgrab, groupscaling
'Dim Shared As String groupgrabhandle
'Dim Shared As Double groupgrabhandlexy(8,2)

	If groupexists=TRUE And buttonson(109)=TRUE And groupscaling=FALSE Then
		infobox("scale with grab handles")
		adjustgextents
		'top left
		line (gextentsx1-detect,gextentsy2-detect)-(gextentsx1+detect,gextentsy2+detect),12,bf
		groupgrabhandlexy(1,1)=gextentsx1
		groupgrabhandlexy(1,2)=gextentsy2
		'top mid
		line (gextentsx1+(gextentsx2-gextentsx1)/2-detect,gextentsy2-detect)-(gextentsx1+(gextentsx2-gextentsx1)/2+detect,gextentsy2+detect),12,bf
		groupgrabhandlexy(2,1)=gextentsx1+(gextentsx2-gextentsx1)/2
		groupgrabhandlexy(2,2)=gextentsy2
		'top right
		line (gextentsx2-detect,gextentsy2-detect)-(gextentsx2+detect,gextentsy2+detect),12,bf
		groupgrabhandlexy(3,1)=gextentsx2
		groupgrabhandlexy(3,2)=gextentsy2
		'right mid
		line (gextentsx2-detect,gextentsy1+(gextentsy2-gextentsy1)/2-detect)-(gextentsx2+detect,gextentsy1+(gextentsy2-gextentsy1)/2+detect),12,bf
		groupgrabhandlexy(4,1)=gextentsx2
		groupgrabhandlexy(4,2)=gextentsy1+(gextentsy2-gextentsy1)/2
		'bottom right
		line (gextentsx2-detect,gextentsy1-detect)-(gextentsx2+detect,gextentsy1+detect),12,bf
		groupgrabhandlexy(5,1)=gextentsx2
		groupgrabhandlexy(5,2)=gextentsy1
		'bottom mid
		line (gextentsx1+(gextentsx2-gextentsx1)/2-detect,gextentsy1-detect)-(gextentsx1+(gextentsx2-gextentsx1)/2+detect,gextentsy1+detect),12,bf
		groupgrabhandlexy(6,1)=gextentsx1+(gextentsx2-gextentsx1)/2
		groupgrabhandlexy(6,2)=gextentsy1
		'bottom left
		line (gextentsx1-detect,gextentsy1-detect)-(gextentsx1+detect,gextentsy1+detect),12,bf
		groupgrabhandlexy(7,1)=gextentsx1
		groupgrabhandlexy(7,2)=gextentsy1
		'left mid
		line (gextentsx1-detect,gextentsy1+(gextentsy2-gextentsy1)/2-detect)-(gextentsx1+detect,gextentsy1+(gextentsy2-gextentsy1)/2+detect),12,bf
		groupgrabhandlexy(8,1)=gextentsx1
		groupgrabhandlexy(8,2)=gextentsy1+(gextentsy2-gextentsy1)/2
		'the group's center point is:
		groupgrabhandlexy(9,1)=gextentsx1+(gextentsx2-gextentsx1)/2
		groupgrabhandlexy(9,2)=gextentsy1+(gextentsy2-gextentsy1)/2
	End If

	If groupscaling=TRUE Then
		'top left
		line (gextentsx1-detect,gextentsy2-detect)-(gextentsx1+detect,gextentsy2+detect),12,bf
		'top mid
		line (gextentsx1+(gextentsx2-gextentsx1)/2-detect,gextentsy2-detect)-(gextentsx1+(gextentsx2-gextentsx1)/2+detect,gextentsy2+detect),12,bf
		'top right
		line (gextentsx2-detect,gextentsy2-detect)-(gextentsx2+detect,gextentsy2+detect),12,bf
		'right mid
		line (gextentsx2-detect,gextentsy1+(gextentsy2-gextentsy1)/2-detect)-(gextentsx2+detect,gextentsy1+(gextentsy2-gextentsy1)/2+detect),12,bf
		'bottom right
		line (gextentsx2-detect,gextentsy1-detect)-(gextentsx2+detect,gextentsy1+detect),12,bf
		'bottom mid
		line (gextentsx1+(gextentsx2-gextentsx1)/2-detect,gextentsy1-detect)-(gextentsx1+(gextentsx2-gextentsx1)/2+detect,gextentsy1+detect),12,bf
		'bottom left
		line (gextentsx1-detect,gextentsy1-detect)-(gextentsx1+detect,gextentsy1+detect),12,bf
		'left mid
		line (gextentsx1-detect,gextentsy1+(gextentsy2-gextentsy1)/2-detect)-(gextentsx1+detect,gextentsy1+(gextentsy2-gextentsy1)/2+detect),12,bf
		'the group's center point is:
	End If
	detectingpoints=1
	If linesingroupc=1 Or circlesingroupc=1 Then
		
	Else
		If selentity=TRUE Then
			Select Case otd
				Case "line"
					If sellinep<>selline Then
						sellinep=selline
						selcirclep=0
						redrawoptionboxes=TRUE
					EndIf
				Case "circle","arc","ellipse","elliptical arc"
					If selcirclep<>selcircle Then
						selcirclep=selcircle
						sellinep=0
						redrawoptionboxes=TRUE
					EndIf
			End Select
		Else
			sellinep=0
			selcirclep=0
		EndIf
		'If optionboxhasdata=TRUE Then
		'	optionboxesredrawn=FALSE
		'	redrawoptionboxes=TRUE
		'	optionboxhasdata=FALSE
		'EndIf
	EndIf
End Sub
sub trackangle()
	if drawatangle=false then
		xlength=x2-x1
		ylength=y2-y1
		fixangle
	end if
End Sub
sub tracklength()
	length = sqr((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)
end sub
sub calcselangle()
	xlength=lines(selline,4)-lines(selline,1)
	ylength=lines(selline,5)-lines(selline,2)
	fixselangle
End Sub
sub calcsellength()
	sellength = sqr((lines(selline,1)-lines(selline,4))^2 + (lines(selline,2)-lines(selline,5))^2 + (lines(selline,3)-lines(selline,6))^2)
End Sub
Sub calcvectorangle(vx1 As Double, vy1 As Double, vx2 As Double, vy2 As double)
	xlength=vx2-vx1
	ylength=vy2-vy1
	fixselangle
End Sub
sub calcperpendicular()
	select case selangle 'angle of selected line to draw perp to.
		case 0,180
			fxm=x1
			fym=lines(selline,2)
		case 90,270
			fxm=lines(selline,1)
			fym=y1
		case else
			perpangle=selangle
			perpangle1=selangle + 90
			memmanageline
			lines(linec,1)=lines(selline,1)
			lines(linec,2)=lines(selline,2)
			lines(linec,4)=x1
			lines(linec,5)=y1
			selline=linec
			calcselangle
			calcsellength
			sellength=sin((perpangle - selangle)*d2r)*sellength
			selangle=perpangle1
			'altosellength
			fxm=x1+cos(selangle*d2r)*sellength
			fym=y1+sin(selangle*d2r)*sellength
			linec=linec-1
			selline=linec
	end select
End Sub
sub altosellength()
	fx=x1+cos(selangle*d2r)*sellength
	fy=y1+sin(selangle*d2r)*sellength
End Sub
Sub flipellipseaxis(flipfilename As String)
	Dim As Double dxf40,dxf41,dxf42,dxfx2,dxfy2
	Dim As Double feax1,feay1,feax2,feay2
	open flipfilename For Input as #4
	Open "tempflipellipseaxis.dat" For Output As #5
	do while not eof(4)
		line input #4, tempstring
		Select Case tempstring
			Case "ELLIPSE"
				Print #5, tempstring'ELLIPSE
				line input #4, tempstring'  5
				Print #5, tempstring
				line input #4, tempstring'68
				Print #5, tempstring
				line input #4, tempstring'100
				Print #5, tempstring
				line input #4, tempstring'AcDbEntity
				Print #5, tempstring
				line input #4, tempstring'  8
				Print #5, tempstring
				line input #4, tempstring'MYLAYER14
				Print #5, tempstring
				line input #4, tempstring'100
				Print #5, tempstring
				line input #4, tempstring'AcDbEllipse
				Print #5, tempstring
				line input #4, tempstring' 10
				Print #5, tempstring
				line input #4, tempstring'260
				Print #5, tempstring
				feax1=Val(tempstring)
				line input #4, tempstring' 20
				Print #5, tempstring
				line input #4, tempstring'157
				Print #5, tempstring
				feay1=Val(tempstring)
				line input #4, tempstring' 30
				Print #5, tempstring
				line input #4, tempstring'0
				Print #5, tempstring
				'pause writing #5 here
				line input #4, tempstring' 11
				line input #4, tempstring'100
				dxfx2=Val(tempstring)
				feax2=feax1+dxfx2
				line input #4, tempstring' 21
				line input #4, tempstring'0
				dxfy2=Val(tempstring)
				feay2=feay1+dxfy2
				line input #4, tempstring' 31
				line input #4, tempstring'0
				line input #4, tempstring'210
				line input #4, tempstring'0.0
				line input #4, tempstring'220
				line input #4, tempstring'0.0
				line input #4, tempstring'230
				line input #4, tempstring'1.0
				line input #4, tempstring' 40
				line input #4, tempstring'.5
				dxf40=Val(tempstring)
				line input #4, tempstring' 41
				line input #4, tempstring'0
				dxf41=Val(tempstring)
				line input #4, tempstring' 42
				line input #4, tempstring'1.57
				dxf42=Val(tempstring)
				
'				'what is the angle from the
'				'ellipse center dxf(10,20)
'				'to the Endpoint of major axis, relative to the center (in WCS)
'				'dxf(11,21)
'				feax1=dxf10
'				feay1=dxf20
'				feax2=dxf11
'				feay2=dxf21
				If dxf40>1 Then
					xlength=feax2-feax1
					ylength=feay2-feay1
					fixangle
					'now the flip major minor axis trick
					angle=angle-90
					If angle<0 then angle=360+angle
					'now what is the length of the axis
					'first we calc length of axis
					length = sqr((feax1-feax2)^2 + (feay1-feay2)^2)
					'based on the ratio of dxf40
					length=length*dxf40
					'calc the new dxf11&21
					feax2=feax1+cos(angle*d2r)*length
					feay2=feay1+sin(angle*d2r)*length
					feax2=feax2-feax1
					feay2=feay2-feay1
					'rotate the start and end angles (in radians) back 90 degrees
					dxf40=1/dxf40
					dxf41=dxf41+1.570796327
					dxf42=dxf42+1.570796327
					
					Print #5, " 11"
					tempstring=str(feax2)
					csntodec
					print #5, tempstring
					Print #5, " 21"
					tempstring=str(feay2)
					csntodec
					print #5, tempstring
				Else
					Print #5, " 11"
					tempstring=str(dxfx2)
					csntodec
					print #5, tempstring
					Print #5, " 21"
					tempstring=str(dxfy2)
					csntodec
					print #5, tempstring
				End If
				Print #5, " 31"
				Print #5, "0"
				Print #5, "210"
				Print #5, "0.0"
				Print #5, "220"
				Print #5, "0.0"
				Print #5, "230"
				Print #5, "1.0"
				Print #5, " 40"
				tempstring=str(dxf40)
				csntodec
				print #5, tempstring
				Print #5, " 41"
				tempstring=str(dxf41)
				csntodec
				print #5, tempstring
				Print #5, " 42"
				tempstring=str(dxf42)
				csntodec
				print #5, tempstring
			Case Else
				Print #5, tempstring
		End select
	loop
	close #4
	Close #5
	Open "tempflipellipseaxis.dat" For Input As #4
	open flipfilename for Output as #5
	Do While Not Eof(4)
		Line Input #4, tempstring
		Print #5, tempstring
	Loop
	Close #4
	Close #5
	
End Sub
sub altox2y2()
	length = sqr((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)
	fx=x1+cos(angle2*d2r)*length
	fy=y1+sin(angle2*d2r)*length
End Sub
sub aftom()
	length = sqr((fxm-mousex)^2 + (fym-mousey)^2 + (z1-z2)^2)
	fx=fxm+cos(angle2*d2r)*length
	fy=fym+sin(angle2*d2r)*length
End Sub
sub altofxm()
	newlength= (fxm-x1) / cos ( angle2*d2r )
	atolength2
End Sub
sub altofym()
	newlength= (fym-y1) / sin ( angle2*d2r )
	atolength2
End Sub
sub atolength()
	fx=x1+cos(angle*d2r)*newlength
	fy=y1+sin(angle*d2r)*newlength
End Sub
sub atolength2()
	fx=x1+cos(angle2*d2r)*newlength
	fy=y1+sin(angle2*d2r)*newlength
End Sub
Sub redraw()
	'Exit Sub
	Dim As Integer i,j,ivi
	Dim As Double splinefx,splinefy
	detect=(wx2-wx1)*detectsize
	detectingpoints=0
	drawingareaupdate=FALSE
	updatefbgfxgtkimage=TRUE
	ScreenCopy 0,1
	ScreenSet 1,1
	window (wx1,wy1)-(wx2,wy2)
	If buttonson(121)=TRUE Then
		'display grid
		Dim As Double wgx,wgy,xi,yi
		wgx=gridxspacing*Int(wx1/gridxspacing)+gridxspacing+gridxoffset
		wgy=gridyspacing*Int(wy1/gridyspacing)+gridyspacing+gridyoffset
		For xi = wgx To wx2 Step gridxspacing
			For yi = wgy To wy2 Step gridyspacing
				PSet(xi,yi),44
				'circle(xi,yi),.5,44
			Next
		Next
	End If
	If buttonson(6)=TRUE Then curvesetup()
	If rayenabled=TRUE Then
		For ivi = inviewlinesc to 1 step -1
			i=inviewlines(ivi)
			Select Case i
				Case rayi To rayi+7
					'draw rays with dashed lines
					linestyle=&b1110000110000111
					line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),lines(i,7),,linestyle
				Case Else
					if lines(i,8)>=0 then line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),lines(i,7)
			End Select
			
		Next
	Else
		For ivi = inviewlinesc to 1 step -1
			i=inviewlines(ivi)
			if lines(i,8)=0 then line (lines(i,1),lines(i,2))-(lines(i,4),lines(i,5)),lines(i,7)
		Next
	EndIf

	For ivi = inviewcirclesc to 1 step -1
		i=inviewcircles(ivi)
		If circles(i,10)=0 Then
			Select Case circles(i,9)
				Case 1
					circle(circles(i,1),circles(i,2)),circles(i,4),circles(i,5)
				Case 2
					circle(circles(i,1),circles(i,2)),circles(i,4),circles(i,5),circles(i,6),circles(i,7)
				Case 3,4
					plotellipse(circles(i,1),circles(i,2),circles(i,3),circles(i,4),Int(circles(i,5)),circles(i,6),circles(i,7),circles(i,8),circles(i,11))
			End Select
		EndIf
	Next
	showgroups
	ScreenCopy 1,2
	ScreenSet 2,2
End Sub
Sub pan()
	viewsset=FALSE
	If panning=FALSE Then
		panx=mousex
		pany=mousey
		panning=TRUE
	else
		wx1=wx1-(mousex-panx)
		wx2=wx2-(mousex-panx)
		wy1=wy1-(mousey-pany)
		wy2=wy2-(mousey-pany)
		inview()
		redraw
		'showgroups
	EndIf
End Sub
Sub panpointtomouse(x As Integer,y As Integer)
	
End Sub
Sub zoomextents()
	viewsset=FALSE
	mousexp=mousex-1
	tempmousex=mousexp-1
	adjustextents
	wx1=extentsx1
	wx2=extentsx2
	wy1=extentsy1
	wy2=extentsy2
	If wx2-wx1<>wy2-wy1 Then
		If wx2-wx1>wy2-wy1 Then
			wy2=wy1+Abs(wx2-wx1)
		Else
			wx2=wx1+Abs(wy2-wy1)
		EndIf
	EndIf
	If wx2-wx1<maxzoomin Then
		wx2=wx1+maxzoomin
		wy2=wy2+maxzoomin
	End If
	wzoom=(wx2-wx1)/wzoomt
	inview()
	redraw
	'showgroups
End Sub
Sub showview()
	viewsset=TRUE
	mousexp=mousex-1
	tempmousex=mousexp-1
	extentsx1=views(viewsi,1)
	extentsy1=views(viewsi,2)
	extentsx2=views(viewsi,3)
	extentsy2=views(viewsi,4)
	wx1=extentsx1
	wx2=extentsx2
	wy1=extentsy1
	wy2=extentsy2
	If wx2-wx1<>wy2-wy1 Then
		If wx2-wx1>wy2-wy1 Then
			wy2=wy1+Abs(wx2-wx1)
		Else
			wx2=wx1+Abs(wy2-wy1)
		EndIf
	EndIf
	If wx2-wx1<maxzoomin Then
		wx2=wx1+maxzoomin
		wy2=wy2+maxzoomin
	End If
	wzoom=(wx2-wx1)/wzoomt
	inview()
	redraw
	'showgroups
End Sub
Sub adjustextents()
	Dim As Integer i
	tempint=0
	For i = 1 To linec
		If lines(i,8)>=0 Then
			tempint=i
			Exit for
		EndIf
	Next
	If tempint=0 Then
		For i = 1 To circlec
			If circles(i,10)>=0 Then
				tempint=i
				Exit for
			EndIf
		Next
		If tempint=0 Then
			extentsx1=0
			extentsx2=drawareax2-drawareax1
			extentsy1=0
			extentsy2=drawareay2-drawareay1
			Exit Sub
		Else
			If circles(tempint,1)-circles(tempint,4)<circles(tempint,1)-circles(tempint,8) Then
				extentsx1=circles(tempint,1)-circles(tempint,4)
				extentsx2=circles(tempint,1)+circles(tempint,4)
			Else
				extentsx1=circles(tempint,1)-circles(tempint,8)
				extentsx2=circles(tempint,1)+circles(tempint,8)
			End If 
			If circles(tempint,2)-circles(tempint,4)<circles(tempint,2)-circles(tempint,8) Then
				extentsy1=circles(tempint,2)-circles(tempint,4)
				extentsy2=circles(tempint,2)+circles(tempint,4)
			Else
				extentsy1=circles(tempint,2)-circles(tempint,8)
				extentsy2=circles(tempint,2)+circles(tempint,8)
			End If 
		End If
	Else
		If lines(tempint,1)<lines(tempint,4) Then
			extentsx1=lines(tempint,1)'-100
			extentsx2=lines(tempint,4)'+100
		Else
			extentsx1=lines(tempint,4)'-100
			extentsx2=lines(tempint,1)'+100
		End If 
		If lines(tempint,2)<lines(tempint,5) Then
			extentsy1=lines(tempint,2)'-100
			extentsy2=lines(tempint,5)'+100
		Else
			extentsy1=lines(tempint,5)'-100
			extentsy2=lines(tempint,2)'+100
		End If
	End if
	For i = 1 To linec
		If lines(i,8)>=0 Then
			If lines(i,1)<extentsx1 Then extentsx1=lines(i,1)'-100
			If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)'-100
			If lines(i,1)>extentsx2 Then extentsx2=lines(i,1)'+100
			If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)'+100
			If lines(i,2)<extentsy1 Then extentsy1=lines(i,2)'-100
			If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)'-100
			If lines(i,2)>extentsy2 Then extentsy2=lines(i,2)'+100
			If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)'+100
		End If
	Next
	For i = 1 To circlec
		If circles(i,10)>=0 Then
			If circles(i,1)-circles(i,4)<extentsx1 Then extentsx1=circles(i,1)-circles(i,4)
			If circles(i,1)-circles(i,8)<extentsx1 Then extentsx1=circles(i,1)-circles(i,8)
			If circles(i,1)+circles(i,4)>extentsx2 Then extentsx2=circles(i,1)+circles(i,4)
			If circles(i,1)+circles(i,8)>extentsx2 Then extentsx2=circles(i,1)+circles(i,8)
			If circles(i,2)-circles(i,4)<extentsy1 Then extentsy1=circles(i,2)-circles(i,4)
			If circles(i,2)-circles(i,8)<extentsy1 Then extentsy1=circles(i,2)-circles(i,8)
			If circles(i,2)+circles(i,4)>extentsy2 Then extentsy2=circles(i,2)+circles(i,4)
			If circles(i,2)+circles(i,8)>extentsy2 Then extentsy2=circles(i,2)+circles(i,8)
		End If
	Next
End sub
Sub adjustgextents()
	If groupexists=FALSE Then Exit Sub
	Dim As Integer i
	tempint=0
	For i = 1 To linec
		If lines(i,8)>=1 Then
			tempint=i
			Exit for
		EndIf
	Next
	If tempint=0 Then
		For i = 1 To circlec
			If circles(i,10)>=1 Then
				tempint=i
				Exit for
			EndIf
		Next
		If tempint=0 Then
			gextentsx1=0
			gextentsx2=drawareax2-drawareax1
			gextentsy1=0
			gextentsy2=drawareay2-drawareay1
			Exit Sub
		Else
			'1(circle)2(arc)3(ellips)4(ellipticalarc)
			Select Case circles(tempint,9)
				Case 1
					gextentsx1=circles(tempint,1)-circles(tempint,4)
					gextentsx2=circles(tempint,1)+circles(tempint,4)
					gextentsy1=circles(tempint,2)-circles(tempint,4)
					gextentsy2=circles(tempint,2)+circles(tempint,4)
				Case 2 To 4
					If circles(tempint,9) = 2 Then
						boxarc(tempint,circles(tempint,1),circles(tempint,2),circles(tempint,3),circles(tempint,4),31,circles(tempint,6)*180/pi,circles(tempint,7)*180/pi,circles(tempint,4),circles(tempint,11))
					Else
						boxarc(tempint,circles(tempint,1),circles(tempint,2),circles(tempint,3),circles(tempint,4),31,circles(tempint,6),circles(tempint,7),circles(tempint,8),circles(tempint,11))
					EndIf
					gextentsx1=boxarcx1
					gextentsy1=boxarcy1
					gextentsx2=boxarcx2
					gextentsy2=boxarcy2
			End Select
			
		End If
	Else
		If lines(tempint,1)<lines(tempint,4) Then
			gextentsx1=lines(tempint,1)'-100
			gextentsx2=lines(tempint,4)'+100
		Else
			gextentsx1=lines(tempint,4)'-100
			gextentsx2=lines(tempint,1)'+100
		End If 
		If lines(tempint,2)<lines(tempint,5) Then
			gextentsy1=lines(tempint,2)'-100
			gextentsy2=lines(tempint,5)'+100
		Else
			gextentsy1=lines(tempint,5)'-100
			gextentsy2=lines(tempint,2)'+100
		End If
	End if
	For i = 1 To linec
		If lines(i,8)>=1 Then
			If lines(i,1)<gextentsx1 Then gextentsx1=lines(i,1)'-100
			If lines(i,4)<gextentsx1 Then gextentsx1=lines(i,4)'-100
			If lines(i,1)>gextentsx2 Then gextentsx2=lines(i,1)'+100
			If lines(i,4)>gextentsx2 Then gextentsx2=lines(i,4)'+100
			If lines(i,2)<gextentsy1 Then gextentsy1=lines(i,2)'-100
			If lines(i,5)<gextentsy1 Then gextentsy1=lines(i,5)'-100
			If lines(i,2)>gextentsy2 Then gextentsy2=lines(i,2)'+100
			If lines(i,5)>gextentsy2 Then gextentsy2=lines(i,5)'+100
		End If
	Next
	For i = 1 To circlec
		If circles(i,10)>=1 Then
			Select Case circles(i,9)
				Case 1
					If circles(i,1)-circles(i,4)<gextentsx1 Then gextentsx1=circles(i,1)-circles(i,4)
					If circles(i,1)+circles(i,4)>gextentsx2 Then gextentsx2=circles(i,1)+circles(i,4)
					If circles(i,2)-circles(i,4)<gextentsy1 Then gextentsy1=circles(i,2)-circles(i,4)
					If circles(i,2)+circles(i,4)>gextentsy2 Then gextentsy2=circles(i,2)+circles(i,4)
				Case 2 To 4
					If circles(i,9) = 2 Then
						boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6)*180/pi,circles(i,7)*180/pi,circles(i,4),circles(i,11))
					Else
						boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
					EndIf
					If boxarcx1<gextentsx1 Then gextentsx1=boxarcx1
					If boxarcx2>gextentsx2 Then gextentsx2=boxarcx2
					If boxarcy1<gextentsy1 Then gextentsy1=boxarcy1
					If boxarcy2>gextentsy2 Then gextentsy2=boxarcy2
			End Select
		End If
	Next
End Sub
sub importdxf()
	Dim As Integer i,j,k
	Dim As Integer templinec,tempcirclec
	Dim As String itemstring
	Dim As Byte loadingblocks
	loadingblocks=FALSE
'	c=0
'	circlec=0
'	redim lines(100,8) as double
	'screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	'blockname="drawing"
	tempstring=openeddrawingname'blockname+".dxf"
	open tempstring for input as #1
	'If Err<>0 Then Exit Sub
	itemstring=""
	do while not eof(1)
		'load arrays
		line input #1, tempstring
		Select Case tempstring
			Case "  0"
				If itemstring = "INSERT" Then
					For i = 1 To blockc-1
						If blocknames(i)=blocknames(blockc) Then
							tempint=linec
							templinec=linec+1
							tempcirclec=circlec+1
							For j=1 To tempint
								If lines(j,9)=i Then
									memmanageline
									For k=1 To 9
										lines(linec,k)=lines(j,k)
									Next
									lines(linec,8)=0
									lines(linec,9)=blockc
								End If
							Next
							tempint=circlec
							For j=1 To tempint
								If circles(j,12)=i Then
									memmanagecircle
									For k=1 To 12
										circles(circlec,k)=circles(j,k)
									Next
									circles(circlec,10)=0
									circles(circlec,12)=blockc
								EndIf
							Next
							modifyx1=blockoffsets(i,1)
							modifyy1=blockoffsets(i,2)
							modifyx2=blockoffsets(blockc,1)
							modifyy2=blockoffsets(blockc,2)
							
							movedxfblock(blockc)
							'this is the rotation and it was working
							'prior to working on scale factors
							If blockoffsets(blockc,7)<>0 Then
								angle=blockoffsets(blockc,7)
								modifyx1=blockoffsets(blockc,1)
								modifyy1=blockoffsets(blockc,2)
								rotateblock(blockc)
							EndIf
							
							If blockoffsets(blockc,4)=0 Then blockoffsets(blockc,4)=1
							If blockoffsets(blockc,5)=0 Then blockoffsets(blockc,5)=1
							If blockoffsets(blockc,6)=0 Then blockoffsets(blockc,6)=1
							If blockoffsets(blockc,4)=1 And blockoffsets(blockc,5)=1 Then
								'do nothing
							Else
								'scale the lines
								'remember to mod this if negative scaling exists
								For j = templinec To linec
									lines(j,1)=blockoffsets(blockc,1)+(lines(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
									lines(j,4)=blockoffsets(blockc,1)+(lines(j,4)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
									lines(j,2)=blockoffsets(blockc,2)+(lines(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
									lines(j,5)=blockoffsets(blockc,2)+(lines(j,5)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
								Next
								'inview()
								'redraw
								'now, for circles there are a lot of things to do
								If blockoffsets(blockc,4)=blockoffsets(blockc,5) Then
									'scale x,y,z all the same
									For j = tempcirclec To circlec
										circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
										circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
										circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
										circles(j,8)=circles(j,8)*blockoffsets(blockc,4)
									Next
								Else
									For j = tempcirclec To circlec
										Select Case circles(j,9)
											Case 1
												'just for circles or arcs (originally)
												'turn them into ellipses here
												'this is not done yet
												circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
												circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
												circles(j,8)=circles(j,4)
												circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
												circles(j,8)=circles(j,8)*blockoffsets(blockc,5)
												circles(j,6)=0
												circles(j,7)=360
												circles(j,9)=3
											Case 2
												circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
												circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
												circles(j,8)=circles(j,4)
												circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
												circles(j,8)=circles(j,8)*blockoffsets(blockc,5)
												circles(j,6)=circles(j,6)*180/pi
												circles(j,7)=circles(j,7)*180/pi
												circles(j,9)=4
											Case 3,4
												'set selcircle to j
												'and pass i to monkeysmatter
												'make sure this works for elliptical arcs
												Select Case circles(j,11)
													Case 0,180,360
														circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
														circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
														circles(j,4)=circles(j,4)*blockoffsets(blockc,4)
														circles(j,8)=circles(j,8)*blockoffsets(blockc,5)
													Case 90,270
														circles(j,1)=blockoffsets(blockc,1)+(circles(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
														circles(j,2)=blockoffsets(blockc,2)+(circles(j,2)-blockoffsets(blockc,2))*blockoffsets(blockc,5)
														circles(j,4)=circles(j,4)*blockoffsets(blockc,5)
														circles(j,8)=circles(j,8)*blockoffsets(blockc,4)
													Case Else
														selcircle=j
														monkeysmatter(i,blockc)
												End Select
										End Select
									Next
								EndIf
							EndIf
							Exit For
						End If
					Next
				End If
				itemstring=""
				'is this at the end of ever insert block
			Case "LINE"
				memmanageline
				itemstring="LINE"
				lines(linec,7)=15
				If loadingblocks=TRUE Then
					lines(linec,8)=-1
					lines(linec,9)=blockc
				EndIf
			Case "CIRCLE"
				memmanagecircle
				circles(circlec,9)=1
				itemstring="CIRCLE"
				circles(circlec,5)=15
				If loadingblocks=TRUE Then
					circles(circlec,10)=-1
					circles(circlec,12)=blockc
				EndIf
			Case "ARC"
				memmanagecircle
				circles(circlec,9)=2
				itemstring="ARC"
				circles(circlec,5)=15
				If loadingblocks=TRUE Then
					circles(circlec,10)=-1
					circles(circlec,12)=blockc
				EndIf
			Case "ELLIPSE"
				memmanagecircle
				itemstring="ELLIPSE"
				circles(circlec,5)=15
				If loadingblocks=TRUE Then
					circles(circlec,10)=-1
					circles(circlec,12)=blockc
				EndIf
			Case "BLOCKS"
				loadingblocks=TRUE
			Case "ENDSEC"
				'this is being used to detect the end of blocks section
				If loadingblocks=TRUE Then loadingblocks=FALSE
			Case "AcDbBlockBegin"
				line input #1, tempstring
				line input #1, tempstring
				Select Case Mid(tempstring,1,12)
					Case "*MODEL_SPACE","*PAPER_SPACE"
					Case Else
						blockc=blockc+1
						ReDim Preserve blocknames(blockc)
						ReDim Preserve blockoffsets(blockc,7)
						ReDim Preserve blockstatus(blockc)
						blockstatus(blockc)=TRUE
						blocknames(blockc)=tempstring
						theboxbelow("Loading block "+tempstring+" with base ")
						itemstring="BLOCK_BASE"
				End Select
			Case "INSERT"
				blockc=blockc+1
				ReDim Preserve blocknames(blockc)
				ReDim Preserve blockoffsets(blockc,7)
				ReDim Preserve blockstatus(blockc)
				itemstring="INSERT"
		End Select
		Select Case itemstring
			Case "INSERT"
				Select Case tempstring
					Case "  2"
						line input #1, tempstring
						blocknames(blockc)=tempstring
						theboxbelow("Inserting block "+tempstring+" ")
					Case " 10"
						line input #1, tempstring
						blockoffsets(blockc,1)=val(tempstring)
						theboxbelow("@ x="+tempstring+" ")
					Case " 20"
						line input #1, tempstring
						blockoffsets(blockc,2)=val(tempstring)
						theboxbelow("y="+tempstring+" ")
					Case " 30"
						line input #1, tempstring
						blockoffsets(blockc,3)=val(tempstring)
						theboxbelow("z="+tempstring+" ")
					Case " 41"
						line input #1, tempstring
						blockoffsets(blockc,4)=val(tempstring)
						theboxbelow("Scale x="+tempstring+" ")
					Case " 42"
						line input #1, tempstring
						blockoffsets(blockc,5)=val(tempstring)
						theboxbelow("y="+tempstring+" ")
					Case " 43"
						line input #1, tempstring
						blockoffsets(blockc,6)=val(tempstring)
						theboxbelow("z="+tempstring+" ")
					Case " 50"
						line input #1, tempstring
						blockoffsets(blockc,7)=val(tempstring)
						theboxbelow("Rotation="+tempstring+crlf)
						'41 is scale x ... a neg one (-1) will flip it horizontally
						'42 is scale y ... a neg one (-1) will flip it vertically
						'43 is scale z
						'the insert block params 50 is rotation
				End Select
			Case "BLOCK_BASE"
				Select Case tempstring
					Case " 10"
						line input #1, tempstring
						blockoffsets(blockc,1)=val(tempstring)
						theboxbelow("@ x="+tempstring+" ")
					Case " 20"
						line input #1, tempstring
						blockoffsets(blockc,2)=val(tempstring)
						theboxbelow("y="+tempstring+" ")
					Case " 30"
						line input #1, tempstring
						blockoffsets(blockc,3)=val(tempstring)
						theboxbelow("z="+tempstring+crlf)
				End Select
			Case "LINE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							lines(linec,7)=val(mid$(tempstring,8))
						Else
							lines(linec,7)=Val(tempstring) Mod 15
							If lines(linec,7)=0 Then lines(linec,7)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						lines(linec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						lines(linec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						lines(linec,3)=val(tempstring)
					Case " 11"
						line input #1, tempstring
						lines(linec,4)=val(tempstring)
					Case " 21"
						line input #1, tempstring
						lines(linec,5)=val(tempstring)
					Case " 31"
						line input #1, tempstring
						lines(linec,6)=val(tempstring)
				End Select
			Case "CIRCLE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 40"
						line input #1, tempstring
						circles(circlec,4)=val(tempstring)
				End Select
			Case "ARC"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 40"
						line input #1, tempstring
						circles(circlec,4)=val(tempstring)
					Case " 50"
						line input #1, tempstring
						circles(circlec,6)=val(tempstring)*d2r
					Case " 51"
						line input #1, tempstring
						circles(circlec,7)=val(tempstring)*d2r
				End Select
			Case "ELLIPSE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 11"
						line input #1, tempstring
						x2=val(tempstring)
					Case " 21"
						line input #1, tempstring
						x1=circles(circlec,1)
						y1=circles(circlec,2)
						y2=val(tempstring)
						x2=x1+x2
						y2=y1+y2
						circles(circlec,4)=(sqr((x1-x2)^2 + (y1-y2)^2))
					Case " 31"
						line input #1, tempstring
					Case " 40"
						line input #1, tempstring
						circles(circlec,8)=circles(circlec,4)*val(tempstring)
						trackangle
						circles(circlec,11)=angle
					Case " 41"
						line input #1, tempstring
						circles(circlec,6)=val(tempstring)*180/pi
					Case " 42"
						line input #1, tempstring
						circles(circlec,7)=val(tempstring)*180/pi
						If circles(circlec,6)=0 And circles(circlec,7)=360 Then
							circles(circlec,9)=3
						Else
							circles(circlec,9)=4
						EndIf
						'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
'						If showellipsefoci=TRUE Then
'							x1=circles(circlec,1)
'							y1=circles(circlec,2)
'							radius=circles(circlec,4)
'							lc=circles(circlec,5)
'							arcstart=circles(circlec,6)
'							arcend=circles(circlec,7)
'							eradius=circles(circlec,8)
'							erotation=circles(circlec,11)
'							createellipse
'							'circles(circlec,10)=1
'							Dim As Double tx1,ty1
'							shift_key=1
'							tx1=x1
'							ty1=y1
'							lc=10
'							If radius>eradius Then
'								foci=Sqr(radius^2-eradius^2)
'								x1=tx1+cos(erotation*d2r)*foci
'								y1=ty1+sin(erotation*d2r)*foci
'								length=2
'								createcircle
'								x1=tx1-cos(erotation*d2r)*foci
'								y1=ty1-sin(erotation*d2r)*foci
'								createcircle
'							Else
'								foci=Sqr(eradius^2-radius^2)
'								x1=tx1+cos((erotation+90)*d2r)*foci
'								y1=ty1+sin((erotation+90)*d2r)*foci
'								length=2
'								createcircle
'								x1=tx1-cos((erotation+90)*d2r)*foci
'								y1=ty1-sin((erotation+90)*d2r)*foci
'								createcircle
'							End If
'						End if

				End Select
		End Select
	loop
	close #1
	zoomextents
End sub
sub forcexyz()
	forcex=true
	fx=fxm
	forcey=true
	fy=fym
	forcez=true
	fz=fzm
end sub
Sub calcmidpoint()
	fxm=(lines(selline,1)+lines(selline,4))/2
	fym=(lines(selline,2)+lines(selline,5))/2
end Sub
Sub calcnearestpoint()
	Dim As Double cnpx1,cnpy1,cnpx2,cnpy2
	Select Case otd
		Case "line"
			If sqr((lines(selline,4)-mousex)^2 + (lines(selline,5)-mousey)^2) < sqr((lines(selline,1)-mousex)^2 + (lines(selline,2)-mousey)^2) Then
				fxm=lines(selline,1)
				fym=lines(selline,2)
				length = sqr((fxm-mousex)^2 + (fym-mousey)^2)
				If length<=sellength Then
					fxm=fxm+cos(selangle*d2r)*length
					fym=fym+sin(selangle*d2r)*length
				Else
					fxm=lines(selline,4)
					fym=lines(selline,5)
				EndIf
			Else
				fxm=lines(selline,4)
				fym=lines(selline,5)
				selangle+=180
				length = sqr((fxm-mousex)^2 + (fym-mousey)^2)
				If length<=sellength Then
					fxm=fxm+cos(selangle*d2r)*length
					fym=fym+sin(selangle*d2r)*length
				Else
					fxm=lines(selline,1)
					fym=lines(selline,2)
				EndIf
			EndIf
		Case "circle"
			xlength=mousex-circles(selcircle,1)
			ylength=mousey-circles(selcircle,2)
			fixangle
			arcend=angle*d2r
			fxm=circles(selcircle,1)+cos(arcend)*circles(selcircle,4)
			fym=circles(selcircle,2)+sin(arcend)*circles(selcircle,4)
		Case "arc"
			xlength=mousex-circles(selcircle,1)
			ylength=mousey-circles(selcircle,2)
			fixangle
			arcend=angle*d2r
			If circles(selcircle,6) < circles(selcircle,7) Then
				Select Case arcend
					Case circles(selcircle,6) To circles(selcircle,7)
						fxm=circles(selcircle,1)+cos(arcend)*circles(selcircle,4)
						fym=circles(selcircle,2)+sin(arcend)*circles(selcircle,4)
					Case Else
						calcarcendpoints
						If calcd(arcendpoint1x,arcendpoint1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(arcendpoint2x,arcendpoint2y,0,CDbl(mousex),CDbl(mousey),0) Then
							fxm=arcendpoint1x
							fym=arcendpoint1y
						Else
							fxm=arcendpoint2x
							fym=arcendpoint2y
						EndIf
				End Select
			Else
				Select Case arcend
					Case circles(selcircle,6) To pi*2, 0 To circles(selcircle,7)
						fxm=circles(selcircle,1)+cos(arcend)*circles(selcircle,4)
						fym=circles(selcircle,2)+sin(arcend)*circles(selcircle,4)
					Case Else
						calcarcendpoints
						If calcd(arcendpoint1x,arcendpoint1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(arcendpoint2x,arcendpoint2y,0,CDbl(mousex),CDbl(mousey),0) Then
							fxm=arcendpoint1x
							fym=arcendpoint1y
						Else
							fxm=arcendpoint2x
							fym=arcendpoint2y
						EndIf
				End Select
			EndIf
		Case "ellipse"
			x1p=mousex-circles(selcircle,1)
			y1p=mousey-circles(selcircle,2)
			cnpx2=x1p*Cos(-circles(selcircle,11)*d2r) - y1p*Sin(-circles(selcircle,11)*d2r)+circles(selcircle,1)
			cnpy2=y1p*Cos(-circles(selcircle,11)*d2r) + x1p*Sin(-circles(selcircle,11)*d2r)+circles(selcircle,2)
			ylength=(cnpy2-circles(selcircle,2))*(circles(selcircle,4)/circles(selcircle,8))
			xlength=cnpx2-circles(selcircle,1)
			fixangle
			x1p=circles(selcircle,1)+(Cos(angle*d2r)*circles(selcircle,4))
			y1p=circles(selcircle,2)+(Sin(angle*d2r)*circles(selcircle,8))
			x1p=x1p-circles(selcircle,1)
			y1p=y1p-circles(selcircle,2)
			fxm=x1p*Cos(circles(selcircle,11)*d2r) - y1p*Sin(circles(selcircle,11)*d2r)+circles(selcircle,1)
			fym=y1p*Cos(circles(selcircle,11)*d2r) + x1p*Sin(circles(selcircle,11)*d2r)+circles(selcircle,2)
		Case "elliptical arc"
			x1p=mousex-circles(selcircle,1)
			y1p=mousey-circles(selcircle,2)
			cnpx2=x1p*Cos(-circles(selcircle,11)*d2r) - y1p*Sin(-circles(selcircle,11)*d2r)+circles(selcircle,1)
			cnpy2=y1p*Cos(-circles(selcircle,11)*d2r) + x1p*Sin(-circles(selcircle,11)*d2r)+circles(selcircle,2)
			ylength=(cnpy2-circles(selcircle,2))*(circles(selcircle,4)/circles(selcircle,8))
			xlength=cnpx2-circles(selcircle,1)
			fixangle
			If circles(selcircle,6) < circles(selcircle,7) Then
				Select Case angle
					Case circles(selcircle,6) To circles(selcircle,7)
						x1p=circles(selcircle,1)+(Cos(angle*d2r)*circles(selcircle,4))
						y1p=circles(selcircle,2)+(Sin(angle*d2r)*circles(selcircle,8))
						x1p=x1p-circles(selcircle,1)
						y1p=y1p-circles(selcircle,2)
						fxm=x1p*Cos(circles(selcircle,11)*d2r) - y1p*Sin(circles(selcircle,11)*d2r)+circles(selcircle,1)
						fym=y1p*Cos(circles(selcircle,11)*d2r) + x1p*Sin(circles(selcircle,11)*d2r)+circles(selcircle,2)
					Case Else
						calcellipsarcendpoints(selcircle)
						If calcd(arcendpoint1x,arcendpoint1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(arcendpoint2x,arcendpoint2y,0,CDbl(mousex),CDbl(mousey),0) Then
							fxm=arcendpoint1x
							fym=arcendpoint1y
						Else
							fxm=arcendpoint2x
							fym=arcendpoint2y
						EndIf
				End Select
			Else
				Select Case angle
					Case circles(selcircle,6) To 360, 0 To circles(selcircle,7)
						x1p=circles(selcircle,1)+(Cos(angle*d2r)*circles(selcircle,4))
						y1p=circles(selcircle,2)+(Sin(angle*d2r)*circles(selcircle,8))
						x1p=x1p-circles(selcircle,1)
						y1p=y1p-circles(selcircle,2)
						fxm=x1p*Cos(circles(selcircle,11)*d2r) - y1p*Sin(circles(selcircle,11)*d2r)+circles(selcircle,1)
						fym=y1p*Cos(circles(selcircle,11)*d2r) + x1p*Sin(circles(selcircle,11)*d2r)+circles(selcircle,2)
					Case Else
						calcellipsarcendpoints(selcircle)
						If calcd(arcendpoint1x,arcendpoint1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(arcendpoint2x,arcendpoint2y,0,CDbl(mousex),CDbl(mousey),0) Then
							fxm=arcendpoint1x
							fym=arcendpoint1y
						Else
							fxm=arcendpoint2x
							fym=arcendpoint2y
						EndIf
				End Select
			EndIf
	End Select
End Sub
Sub turn_gtk_button_on()
	Select Case selbutton
		Case 1
			g_object_ref(bae1gbtnimgdn1)
			gtk_button_set_image(bae1gb1, bae1gbtnimgdn1)
		Case 2
			g_object_ref(bae1gbtnimgdn2)
			gtk_button_set_image(bae1gb2, bae1gbtnimgdn2)
		Case 3
			g_object_ref(bae1gbtnimgdn3)
			gtk_button_set_image(bae1gb3, bae1gbtnimgdn3)
		Case 4
			g_object_ref(bae1gbtnimgdn4)
			gtk_button_set_image(bae1gb4, bae1gbtnimgdn4)
		Case 5
			g_object_ref(bae1gbtnimgdn5)
			gtk_button_set_image(bae1gb5, bae1gbtnimgdn5)
		Case 6
			g_object_ref(bae1gbtnimgdn6)
			gtk_button_set_image(bae1gb6, bae1gbtnimgdn6)
		Case 1001
			g_object_ref(bae2gbtnimgdn1)
			gtk_button_set_image(bae2gb1, bae2gbtnimgdn1)
		Case 1002
			g_object_ref(bae2gbtnimgdn2)
			gtk_button_set_image(bae2gb2, bae2gbtnimgdn2)
		Case 1003
			g_object_ref(bae2gbtnimgdn3)
			gtk_button_set_image(bae2gb3, bae2gbtnimgdn3)
		Case 1004
			g_object_ref(bae2gbtnimgdn4)
			gtk_button_set_image(bae2gb4, bae2gbtnimgdn4)
		Case 1005
			g_object_ref(bae2gbtnimgdn5)
			gtk_button_set_image(bae2gb5, bae2gbtnimgdn5)
		Case 1006
			g_object_ref(bae2gbtnimgdn6)
			gtk_button_set_image(bae2gb6, bae2gbtnimgdn6)
		Case 1007
			g_object_ref(bae2gbtnimgdn7)
			gtk_button_set_image(bae2gb7, bae2gbtnimgdn7)
		Case 1008
			g_object_ref(bae2gbtnimgdn8)
			gtk_button_set_image(bae2gb8, bae2gbtnimgdn8)
		Case 1009
			g_object_ref(bae2gbtnimgdn9)
			gtk_button_set_image(bae2gb9, bae2gbtnimgdn9)
		Case 1010
			g_object_ref(bae2gbtnimgdn10)
			gtk_button_set_image(bae2gb10, bae2gbtnimgdn10)


		Case 11
			g_object_ref(bae3gbtnimgdn1)
			gtk_button_set_image(bae3gb1, bae3gbtnimgdn1)
		Case 12
			g_object_ref(bae3gbtnimgdn2)
			gtk_button_set_image(bae3gb2, bae3gbtnimgdn2)
		Case 13
			g_object_ref(bae3gbtnimgdn3)
			gtk_button_set_image(bae3gb3, bae3gbtnimgdn3)
		Case 14
			g_object_ref(bae3gbtnimgdn4)
			gtk_button_set_image(bae3gb4, bae3gbtnimgdn4)
		Case 16
			g_object_ref(bae3gbtnimgdn5)
			gtk_button_set_image(bae3gb5, bae3gbtnimgdn5)
		Case 18
			g_object_ref(bae3gbtnimgdn6)
			gtk_button_set_image(bae3gb6, bae3gbtnimgdn6)
		Case 19
			g_object_ref(bae3gbtnimgdn7)
			gtk_button_set_image(bae3gb7, bae3gbtnimgdn7)
		Case 20
			g_object_ref(bae3gbtnimgdn8)
			gtk_button_set_image(bae3gb8, bae3gbtnimgdn8)


		Case 21
			g_object_ref(bae4gbtnimgdn1)
			gtk_button_set_image(bae4gb1, bae4gbtnimgdn1)
		Case 22
			g_object_ref(bae4gbtnimgdn2)
			gtk_button_set_image(bae4gb2, bae4gbtnimgdn2)
		Case 24
			g_object_ref(bae4gbtnimgdn4)
			gtk_button_set_image(bae4gb4, bae4gbtnimgdn4)
		Case 25
			g_object_ref(bae4gbtnimgdn5)
			gtk_button_set_image(bae4gb5, bae4gbtnimgdn5)
		Case 26
			g_object_ref(bae4gbtnimgdn6)
			gtk_button_set_image(bae4gb6, bae4gbtnimgdn6)
		Case 27
			g_object_ref(bae4gbtnimgdn7)
			gtk_button_set_image(bae4gb7, bae4gbtnimgdn7)
		Case 28
			g_object_ref(bae4gbtnimgdn8)
			gtk_button_set_image(bae4gb8, bae4gbtnimgdn8)
		Case 53
			g_object_ref(bae4gbtnimgdn9)
			gtk_button_set_image(bae4gb9, bae4gbtnimgdn9)
		Case 54
			g_object_ref(bae4gbtnimgdn10)
			gtk_button_set_image(bae4gb10, bae4gbtnimgdn10)



		Case 31
			g_object_ref(bae5gbtnimgdn1)
			gtk_button_set_image(bae5gb1, bae5gbtnimgdn1)
		Case 32
			g_object_ref(bae5gbtnimgdn2)
			gtk_button_set_image(bae5gb2, bae5gbtnimgdn2)
		Case 33
			g_object_ref(bae5gbtnimgdn3)
			gtk_button_set_image(bae5gb3, bae5gbtnimgdn3)
		Case 34
			g_object_ref(bae5gbtnimgdn4)
			gtk_button_set_image(bae5gb4, bae5gbtnimgdn4)
		Case 35
			g_object_ref(bae5gbtnimgdn5)
			gtk_button_set_image(bae5gb5, bae5gbtnimgdn5)
		Case 36
			g_object_ref(bae5gbtnimgdn6)
			gtk_button_set_image(bae5gb6, bae5gbtnimgdn6)
		Case 37
			g_object_ref(bae5gbtnimgdn7)
			gtk_button_set_image(bae5gb7, bae5gbtnimgdn7)
		Case 38
			g_object_ref(bae5gbtnimgdn8)
			gtk_button_set_image(bae5gb8, bae5gbtnimgdn8)
		Case 39
			g_object_ref(bae5gbtnimgdn9)
			gtk_button_set_image(bae5gb9, bae5gbtnimgdn9)
		Case 40
			g_object_ref(bae5gbtnimgdn10)
			gtk_button_set_image(bae5gb10, bae5gbtnimgdn10)


		Case 41
			g_object_ref(bae6gbtnimgdn1)
			gtk_button_set_image(bae6gb1, bae6gbtnimgdn1)
		Case 42
			g_object_ref(bae6gbtnimgdn2)
			gtk_button_set_image(bae6gb2, bae6gbtnimgdn2)
		Case 43
			g_object_ref(bae6gbtnimgdn3)
			gtk_button_set_image(bae6gb3, bae6gbtnimgdn3)
		Case 44
			g_object_ref(bae6gbtnimgdn4)
			gtk_button_set_image(bae6gb4, bae6gbtnimgdn4)
		Case 45
			g_object_ref(bae6gbtnimgdn5)
			gtk_button_set_image(bae6gb5, bae6gbtnimgdn5)
		Case 46
			g_object_ref(bae6gbtnimgdn6)
			gtk_button_set_image(bae6gb6, bae6gbtnimgdn6)
		Case 47
			g_object_ref(bae6gbtnimgdn7)
			gtk_button_set_image(bae6gb7, bae6gbtnimgdn7)
		Case 48
			g_object_ref(bae6gbtnimgdn8)
			gtk_button_set_image(bae6gb8, bae6gbtnimgdn8)
		Case 49
			g_object_ref(bae6gbtnimgdn9)
			gtk_button_set_image(bae6gb9, bae6gbtnimgdn9)
		Case 50
			g_object_ref(bae6gbtnimgdn10)
			gtk_button_set_image(bae6gb10, bae6gbtnimgdn10)


		Case 71
			g_object_ref(bae7gbtnimgdn1)
			gtk_button_set_image(bae7gb1, bae7gbtnimgdn1)
		Case 72
			g_object_ref(bae7gbtnimgdn2)
			gtk_button_set_image(bae7gb2, bae7gbtnimgdn2)
		Case 73
			g_object_ref(bae7gbtnimgdn3)
			gtk_button_set_image(bae7gb3, bae7gbtnimgdn3)
		Case 74
			g_object_ref(bae7gbtnimgdn4)
			gtk_button_set_image(bae7gb4, bae7gbtnimgdn4)
		Case 75
			g_object_ref(bae7gbtnimgdn5)
			gtk_button_set_image(bae7gb5, bae7gbtnimgdn5)
		Case 76
			g_object_ref(bae7gbtnimgdn6)
			gtk_button_set_image(bae7gb6, bae7gbtnimgdn6)


		Case 81
			g_object_ref(bae8gbtnimgdn1)
			gtk_button_set_image(bae8gb1, bae8gbtnimgdn1)
		Case 82
			g_object_ref(bae8gbtnimgdn2)
			gtk_button_set_image(bae8gb2, bae8gbtnimgdn2)
		Case 83
			g_object_ref(bae8gbtnimgdn3)
			gtk_button_set_image(bae8gb3, bae8gbtnimgdn3)
		Case 84
			g_object_ref(bae8gbtnimgdn4)
			gtk_button_set_image(bae8gb4, bae8gbtnimgdn4)
		Case 86
			g_object_ref(bae8gbtnimgdn6)
			gtk_button_set_image(bae8gb6, bae8gbtnimgdn6)
		Case 87
			g_object_ref(bae8gbtnimgdn7)
			gtk_button_set_image(bae8gb7, bae8gbtnimgdn7)
		Case 88
			g_object_ref(bae8gbtnimgdn8)
			gtk_button_set_image(bae8gb8, bae8gbtnimgdn8)
		Case 89
			g_object_ref(bae8gbtnimgdn9)
			gtk_button_set_image(bae8gb9, bae8gbtnimgdn9)
		Case 90
			g_object_ref(bae8gbtnimgdn10)
			gtk_button_set_image(bae8gb10, bae8gbtnimgdn10)



		Case 103
			g_object_ref(bae9gbtnimgdn2)
			gtk_button_set_image(bae9gb2, bae9gbtnimgdn2)
		Case 104
			g_object_ref(bae9gbtnimgdn3)
			gtk_button_set_image(bae9gb3, bae9gbtnimgdn3)
		Case 109
			g_object_ref(bae9gbtnimgdn4)
			gtk_button_set_image(bae9gb4, bae9gbtnimgdn4)

		Case 116
			g_object_ref(bae10gbtnimgdn4)
			gtk_button_set_image(bae10gb4, bae10gbtnimgdn4)

		Case 121
			g_object_ref(bae11gbtnimgdn1)
			gtk_button_set_image(bae11gb1, bae11gbtnimgdn1)
		Case 122
			g_object_ref(bae11gbtnimgdn2)
			gtk_button_set_image(bae11gb2, bae11gbtnimgdn2)


		Case 141
			g_object_ref(bae12gbtnimgdn1)
			gtk_button_set_image(bae12gb1, bae12gbtnimgdn1)
		Case 142
			g_object_ref(bae12gbtnimgdn2)
			gtk_button_set_image(bae12gb2, bae12gbtnimgdn2)
		Case 143
			g_object_ref(bae12gbtnimgdn3)
			gtk_button_set_image(bae12gb3, bae12gbtnimgdn3)
		Case 144
			g_object_ref(bae12gbtnimgdn4)
			gtk_button_set_image(bae12gb4, bae12gbtnimgdn4)


		Case 51
			g_object_ref(bae13gbtnimgdn1)
			gtk_button_set_image(bae13gb1, bae13gbtnimgdn1)
		Case 52
			g_object_ref(bae13gbtnimgdn2)
			gtk_button_set_image(bae13gb2, bae13gbtnimgdn2)
		Case 61
			g_object_ref(bae13gbtnimgdn3)
			gtk_button_set_image(bae13gb3, bae13gbtnimgdn3)
		Case 62
			g_object_ref(bae13gbtnimgdn4)
			gtk_button_set_image(bae13gb4, bae13gbtnimgdn4)


		Case 55
			g_object_ref(bae14gbtnimgdn1)
			gtk_button_set_image(bae14gb1, bae14gbtnimgdn1)
		Case 56
			g_object_ref(bae14gbtnimgdn2)
			gtk_button_set_image(bae14gb2, bae14gbtnimgdn2)
		Case 58
			g_object_ref(bae14gbtnimgdn4)
			gtk_button_set_image(bae14gb4, bae14gbtnimgdn4)
		Case 59
			g_object_ref(bae14gbtnimgdn5)
			gtk_button_set_image(bae14gb5, bae14gbtnimgdn5)
		Case 60
			g_object_ref(bae14gbtnimgdn6)
			gtk_button_set_image(bae14gb6, bae14gbtnimgdn6)
		Case 65
			g_object_ref(bae14gbtnimgdn7)
			gtk_button_set_image(bae14gb7, bae14gbtnimgdn7)
		Case 67
			g_object_ref(bae14gbtnimgdn8)
			gtk_button_set_image(bae14gb8, bae14gbtnimgdn8)
		Case 66
			g_object_ref(bae14gbtnimgdn9)
			gtk_button_set_image(bae14gb9, bae14gbtnimgdn9)


		Case 93
			g_object_ref(bae15gbtnimgdn3)
			gtk_button_set_image(bae15gb3, bae15gbtnimgdn3)
		Case 94
			g_object_ref(bae15gbtnimgdn4)
			gtk_button_set_image(bae15gb4, bae15gbtnimgdn4)
		Case 95
			g_object_ref(bae15gbtnimgdn5)
			gtk_button_set_image(bae15gb5, bae15gbtnimgdn5)
		Case 96
			g_object_ref(bae15gbtnimgdn6)
			gtk_button_set_image(bae15gb6, bae15gbtnimgdn6)
		Case 97
			g_object_ref(bae15gbtnimgdn7)
			gtk_button_set_image(bae15gb7, bae15gbtnimgdn7)



	End Select
	gtk_widget_grab_focus(ebox)
End Sub
Sub turn_gtk_button_off()
	Select Case selbutton
		Case 1
			g_object_ref(bae1gbtnimgup1)
			gtk_button_set_image(bae1gb1, bae1gbtnimgup1)
		Case 2
			g_object_ref(bae1gbtnimgup2)
			gtk_button_set_image(bae1gb2, bae1gbtnimgup2)
		Case 3
			g_object_ref(bae1gbtnimgup3)
			gtk_button_set_image(bae1gb3, bae1gbtnimgup3)
		Case 4
			g_object_ref(bae1gbtnimgup4)
			gtk_button_set_image(bae1gb4, bae1gbtnimgup4)
		Case 5
			g_object_ref(bae1gbtnimgup5)
			gtk_button_set_image(bae1gb5, bae1gbtnimgup5)
		Case 6
			g_object_ref(bae1gbtnimgup6)
			gtk_button_set_image(bae1gb6, bae1gbtnimgup6)
		Case 1001
			g_object_ref(bae2gbtnimgup1)
			gtk_button_set_image(bae2gb1, bae2gbtnimgup1)
		Case 1002
			g_object_ref(bae2gbtnimgup2)
			gtk_button_set_image(bae2gb2, bae2gbtnimgup2)
		Case 1003
			g_object_ref(bae2gbtnimgup3)
			gtk_button_set_image(bae2gb3, bae2gbtnimgup3)
		Case 1004
			g_object_ref(bae2gbtnimgup4)
			gtk_button_set_image(bae2gb4, bae2gbtnimgup4)
		Case 1005
			g_object_ref(bae2gbtnimgup5)
			gtk_button_set_image(bae2gb5, bae2gbtnimgup5)
		Case 1006
			g_object_ref(bae2gbtnimgup6)
			gtk_button_set_image(bae2gb6, bae2gbtnimgup6)
		Case 1007
			g_object_ref(bae2gbtnimgup7)
			gtk_button_set_image(bae2gb7, bae2gbtnimgup7)
		Case 1008
			g_object_ref(bae2gbtnimgup8)
			gtk_button_set_image(bae2gb8, bae2gbtnimgup8)
		Case 1009
			g_object_ref(bae2gbtnimgup9)
			gtk_button_set_image(bae2gb9, bae2gbtnimgup9)
		Case 1010
			g_object_ref(bae2gbtnimgup10)
			gtk_button_set_image(bae2gb10, bae2gbtnimgup10)


		Case 11
			g_object_ref(bae3gbtnimgup1)
			gtk_button_set_image(bae3gb1, bae3gbtnimgup1)
		Case 12
			g_object_ref(bae3gbtnimgup2)
			gtk_button_set_image(bae3gb2, bae3gbtnimgup2)
		Case 13
			g_object_ref(bae3gbtnimgup3)
			gtk_button_set_image(bae3gb3, bae3gbtnimgup3)
		Case 14
			g_object_ref(bae3gbtnimgup4)
			gtk_button_set_image(bae3gb4, bae3gbtnimgup4)
		Case 16
			g_object_ref(bae3gbtnimgup5)
			gtk_button_set_image(bae3gb5, bae3gbtnimgup5)
		Case 18
			g_object_ref(bae3gbtnimgup6)
			gtk_button_set_image(bae3gb6, bae3gbtnimgup6)
		Case 19
			g_object_ref(bae3gbtnimgup7)
			gtk_button_set_image(bae3gb7, bae3gbtnimgup7)
		Case 20
			g_object_ref(bae3gbtnimgup8)
			gtk_button_set_image(bae3gb8, bae3gbtnimgup8)


		Case 21
			g_object_ref(bae4gbtnimgup1)
			gtk_button_set_image(bae4gb1, bae4gbtnimgup1)
		Case 22
			g_object_ref(bae4gbtnimgup2)
			gtk_button_set_image(bae4gb2, bae4gbtnimgup2)
		Case 24
			g_object_ref(bae4gbtnimgup4)
			gtk_button_set_image(bae4gb4, bae4gbtnimgup4)
		Case 25
			g_object_ref(bae4gbtnimgup5)
			gtk_button_set_image(bae4gb5, bae4gbtnimgup5)
		Case 26
			g_object_ref(bae4gbtnimgup6)
			gtk_button_set_image(bae4gb6, bae4gbtnimgup6)
		Case 27
			g_object_ref(bae4gbtnimgup7)
			gtk_button_set_image(bae4gb7, bae4gbtnimgup7)
		Case 28
			g_object_ref(bae4gbtnimgup8)
			gtk_button_set_image(bae4gb8, bae4gbtnimgup8)
		Case 53
			g_object_ref(bae4gbtnimgup9)
			gtk_button_set_image(bae4gb9, bae4gbtnimgup9)
		Case 54
			g_object_ref(bae4gbtnimgup10)
			gtk_button_set_image(bae4gb10, bae4gbtnimgup10)


		Case 31
			g_object_ref(bae5gbtnimgup1)
			gtk_button_set_image(bae5gb1, bae5gbtnimgup1)
		Case 32
			g_object_ref(bae5gbtnimgup2)
			gtk_button_set_image(bae5gb2, bae5gbtnimgup2)
		Case 33
			g_object_ref(bae5gbtnimgup3)
			gtk_button_set_image(bae5gb3, bae5gbtnimgup3)
		Case 34
			g_object_ref(bae5gbtnimgup4)
			gtk_button_set_image(bae5gb4, bae5gbtnimgup4)
		Case 35
			g_object_ref(bae5gbtnimgup5)
			gtk_button_set_image(bae5gb5, bae5gbtnimgup5)
		Case 36
			g_object_ref(bae5gbtnimgup6)
			gtk_button_set_image(bae5gb6, bae5gbtnimgup6)
		Case 37
			g_object_ref(bae5gbtnimgup7)
			gtk_button_set_image(bae5gb7, bae5gbtnimgup7)
		Case 38
			g_object_ref(bae5gbtnimgup8)
			gtk_button_set_image(bae5gb8, bae5gbtnimgup8)
		Case 39
			g_object_ref(bae5gbtnimgup9)
			gtk_button_set_image(bae5gb9, bae5gbtnimgup9)
		Case 40
			g_object_ref(bae5gbtnimgup10)
			gtk_button_set_image(bae5gb10, bae5gbtnimgup10)


		Case 41
			g_object_ref(bae6gbtnimgup1)
			gtk_button_set_image(bae6gb1, bae6gbtnimgup1)
		Case 42
			g_object_ref(bae6gbtnimgup2)
			gtk_button_set_image(bae6gb2, bae6gbtnimgup2)
		Case 43
			g_object_ref(bae6gbtnimgup3)
			gtk_button_set_image(bae6gb3, bae6gbtnimgup3)
		Case 44
			g_object_ref(bae6gbtnimgup4)
			gtk_button_set_image(bae6gb4, bae6gbtnimgup4)
		Case 45
			g_object_ref(bae6gbtnimgup5)
			gtk_button_set_image(bae6gb5, bae6gbtnimgup5)
		Case 46
			g_object_ref(bae6gbtnimgup6)
			gtk_button_set_image(bae6gb6, bae6gbtnimgup6)
		Case 47
			g_object_ref(bae6gbtnimgup7)
			gtk_button_set_image(bae6gb7, bae6gbtnimgup7)
		Case 48
			g_object_ref(bae6gbtnimgup8)
			gtk_button_set_image(bae6gb8, bae6gbtnimgup8)
		Case 49
			g_object_ref(bae6gbtnimgup9)
			gtk_button_set_image(bae6gb9, bae6gbtnimgup9)
		Case 50
			g_object_ref(bae6gbtnimgup10)
			gtk_button_set_image(bae6gb10, bae6gbtnimgup10)



		Case 71
			g_object_ref(bae7gbtnimgup1)
			gtk_button_set_image(bae7gb1, bae7gbtnimgup1)
		Case 72
			g_object_ref(bae7gbtnimgup2)
			gtk_button_set_image(bae7gb2, bae7gbtnimgup2)
		Case 73
			g_object_ref(bae7gbtnimgup3)
			gtk_button_set_image(bae7gb3, bae7gbtnimgup3)
		Case 74
			g_object_ref(bae7gbtnimgup4)
			gtk_button_set_image(bae7gb4, bae7gbtnimgup4)
		Case 75
			g_object_ref(bae7gbtnimgup5)
			gtk_button_set_image(bae7gb5, bae7gbtnimgup5)
		Case 76
			g_object_ref(bae7gbtnimgup6)
			gtk_button_set_image(bae7gb6, bae7gbtnimgup6)



		Case 81
			g_object_ref(bae8gbtnimgup1)
			gtk_button_set_image(bae8gb1, bae8gbtnimgup1)
		Case 82
			g_object_ref(bae8gbtnimgup2)
			gtk_button_set_image(bae8gb2, bae8gbtnimgup2)
		Case 83
			g_object_ref(bae8gbtnimgup3)
			gtk_button_set_image(bae8gb3, bae8gbtnimgup3)
		Case 84
			g_object_ref(bae8gbtnimgup4)
			gtk_button_set_image(bae8gb4, bae8gbtnimgup4)
		Case 86
			g_object_ref(bae8gbtnimgup6)
			gtk_button_set_image(bae8gb6, bae8gbtnimgup6)
		Case 87
			g_object_ref(bae8gbtnimgup7)
			gtk_button_set_image(bae8gb7, bae8gbtnimgup7)
		Case 88
			g_object_ref(bae8gbtnimgup8)
			gtk_button_set_image(bae8gb8, bae8gbtnimgup8)
		Case 89
			g_object_ref(bae8gbtnimgup9)
			gtk_button_set_image(bae8gb9, bae8gbtnimgup9)
		Case 90
			g_object_ref(bae8gbtnimgup10)
			gtk_button_set_image(bae8gb10, bae8gbtnimgup10)



		Case 103
			g_object_ref(bae9gbtnimgup2)
			gtk_button_set_image(bae9gb2, bae9gbtnimgup2)
		Case 104
			g_object_ref(bae9gbtnimgup3)
			gtk_button_set_image(bae9gb3, bae9gbtnimgup3)
		Case 109
			g_object_ref(bae9gbtnimgup4)
			gtk_button_set_image(bae9gb4, bae9gbtnimgup4)

		Case 116
			g_object_ref(bae10gbtnimgup4)
			gtk_button_set_image(bae10gb4, bae10gbtnimgup4)

		Case 121
			g_object_ref(bae11gbtnimgup1)
			gtk_button_set_image(bae11gb1, bae11gbtnimgup1)
		Case 122
			g_object_ref(bae11gbtnimgup2)
			gtk_button_set_image(bae11gb2, bae11gbtnimgup2)


		Case 141
			g_object_ref(bae12gbtnimgup1)
			gtk_button_set_image(bae12gb1, bae12gbtnimgup1)
		Case 142
			g_object_ref(bae12gbtnimgup2)
			gtk_button_set_image(bae12gb2, bae12gbtnimgup2)
		Case 143
			g_object_ref(bae12gbtnimgup3)
			gtk_button_set_image(bae12gb3, bae12gbtnimgup3)
		Case 144
			g_object_ref(bae12gbtnimgup4)
			gtk_button_set_image(bae12gb4, bae12gbtnimgup4)


		Case 51
			g_object_ref(bae13gbtnimgup1)
			gtk_button_set_image(bae13gb1, bae13gbtnimgup1)
		Case 52
			g_object_ref(bae13gbtnimgup2)
			gtk_button_set_image(bae13gb2, bae13gbtnimgup2)
		Case 61
			g_object_ref(bae13gbtnimgup3)
			gtk_button_set_image(bae13gb3, bae13gbtnimgup3)
		Case 62
			g_object_ref(bae13gbtnimgup4)
			gtk_button_set_image(bae13gb4, bae13gbtnimgup4)



		Case 55
			g_object_ref(bae14gbtnimgup1)
			gtk_button_set_image(bae14gb1, bae14gbtnimgup1)
		Case 56
			g_object_ref(bae14gbtnimgup2)
			gtk_button_set_image(bae14gb2, bae14gbtnimgup2)
		Case 58
			g_object_ref(bae14gbtnimgup4)
			gtk_button_set_image(bae14gb4, bae14gbtnimgup4)
		Case 59
			g_object_ref(bae14gbtnimgup5)
			gtk_button_set_image(bae14gb5, bae14gbtnimgup5)
		Case 60
			g_object_ref(bae14gbtnimgup6)
			gtk_button_set_image(bae14gb6, bae14gbtnimgup6)
		Case 65
			g_object_ref(bae14gbtnimgup7)
			gtk_button_set_image(bae14gb7, bae14gbtnimgup7)
		Case 67
			g_object_ref(bae14gbtnimgup8)
			gtk_button_set_image(bae14gb8, bae14gbtnimgup8)
		Case 66
			g_object_ref(bae14gbtnimgup9)
			gtk_button_set_image(bae14gb9, bae14gbtnimgup9)


		Case 93
			g_object_ref(bae15gbtnimgup3)
			gtk_button_set_image(bae15gb3, bae15gbtnimgup3)
		Case 94
			g_object_ref(bae15gbtnimgup4)
			gtk_button_set_image(bae15gb4, bae15gbtnimgup4)
		Case 95
			g_object_ref(bae15gbtnimgup5)
			gtk_button_set_image(bae15gb5, bae15gbtnimgup5)
		Case 96
			g_object_ref(bae15gbtnimgup6)
			gtk_button_set_image(bae15gb6, bae15gbtnimgup6)
		Case 97
			g_object_ref(bae15gbtnimgup7)
			gtk_button_set_image(bae15gb7, bae15gbtnimgup7)



	End Select
	gtk_widget_grab_focus(ebox)
End Sub
sub turnbuttonoff()
	'sometimes this routine is called from with in a for next loop
	'in order to turn off (disable) a series of buttons prior to enabling some other button (drawing function).
	'turnbuttonoff2 is used is when the user clicks an individual button off
	Dim As Integer i,tempselbutton
	If selbutton=51 And splining=TRUE Then
		selbutton=6
		turnbuttonon()
		selbutton=51
	EndIf
	buttonson(selbutton)=FALSE
	turn_gtk_button_off
	select case selbutton
		case 11 to 20
			snapenable=false
		case 21
			if ortho=true then orthomode
		Case 22,53,54
			drawatangle=false
			forcex=FALSE
			forcey=FALSE
			angle2=0
		Case 81,82
			modify=0
			modifying=FALSE
			useusersetcflength=FALSE
			cpe=FALSE
			selline=0
			selcircle=0
		Case 84
			useusersetcflength=TRUE
		Case 121'turn grid visibility off
			redraw
			'showgroups
		Case 141
			redraw
	End Select
end sub
sub turnbuttonoff2()
	'user clicks an individual button off
	Dim As Integer i,tempselbutton
	If selbutton=51 And splining=TRUE Then
		selbutton=6
		turnbuttonon()
		selbutton=51
	EndIf
	buttonson(selbutton)=false
	turn_gtk_button_off
	select case selbutton
		case 11 to 20
			snapenable=false
		case 21
			if ortho=true then orthomode
		Case 22,53,54
			drawatangle=false
			forcex=false
			forcey=FALSE
			angle2=0
		Case 81,82
			modify=0
			modifying=FALSE
			useusersetcflength=FALSE
			cpe=FALSE
			selline=0
			selcircle=0
		Case 84
			useusersetcflength=TRUE
		Case 85 To 90
			tempselbutton=selbutton
			for i = 81 to 82
				selbutton=i
				turnbuttonoff
			Next
			selbutton=tempselbutton
		Case 109
			redraw
		Case 121'turn grid visibility off
			redraw
			'showgroups
		Case 141
			redraw
	End Select
end sub
Sub turnbuttonon()
	Dim As Integer i,j,k,c,flashoff
	flashoff=0
	'to print something on the screen and or get input then:
	'set screen 1,1
	'then print or input
	'then set screen back to 0,0
	'actually if just pringing on screen
	'there's no need to set screen back to 0,0
	'cuz that'll happen as soon a user moves mouse out of specific button
	'by default when user clicks a button the button is to remain on
	'this is by default becuase flashoff=0
	'but if you just want the user to click the button
	'and simply display a message and or imput info
	'and not have the button enabled then set flashoff=1
	'
	'now i need to address the idea of user locking up the program by
	'randomly clicking on buttons
	'i need to make this crash proof.
	'anoter thing is user input needs to be put in an input dialog
	'so user is not left wondering why nothing is happening
	'using the input command halts program till completed so that'll work
	'for his purpose
	
	If selbutton <> 61 Then
		tempint=selbutton
		selbutton=61
		turnbuttonoff
		selbutton=tempint
	End if
	select case selbutton
		case 1 to 10,1001 To 1010',43
			escapeall
			If selbutton=6 Then
				If splining=FALSE Then
					splinec=0
					splining=TRUE
				EndIf
			EndIf
			tempint=selbutton
			for i = 1 to 10
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			for i = 1001 to 1010
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			for i = 61 to 70
				selbutton=i
				turnbuttonoff
			Next
			for i = 71 to 78
				selbutton=i
				turnbuttonoff
			Next
			selbutton=80
			turnbuttonoff
			for i = 81 to 83
				selbutton=i
				turnbuttonoff
			Next
			selbutton=tempint
		case 11 to 20
			tempint=selbutton
			for i = 11 to 20
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			next
			selbutton=tempint
			snapenable=true
		case 21
			if ortho=false then orthomode
		case 22
			drawatangle=TRUE
		Case 24 To 28
			tempint=selbutton
			for i = 24 to 28
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			selbutton=tempint
			angle2=usersetangles(selbutton-23)
		case 31,32,35 to 40
			if groupexists=false then exit Sub
		Case 33,34
			rotao=0
			if groupexists=false then exit Sub
		Case 41
			zoomin
			Exit Sub'instead of exiting sub just set flashoff=1
		Case 42
			zoomout
			Exit Sub'instead of exiting sub just set flashoff=1
		Case 44
			zoomextents
			Exit sub'instead of exiting sub just set flashoff=1
'Data 45,"S","off","Save view","Saves current view"
'Data 46,"<","off","Previous view","Changes veiw to previous saved view"
'Data 47,">","off","Next view","Changes veiw to next saved veiw"
'Data 48,"F","off","First view","Changes veiw to first saved veiw"
'Data 49,"L","off","Last view","Changes veiw to last saved veiw"
'Data 50,"d","off","Delete view","Deletes current saved view if viewing a saved view"
		Case 45'save view
			If viewsset=FALSE Then
				memmanageviews
				viewsi=viewsc
				views(viewsc,1)=wx1
				views(viewsc,2)=wy1
				views(viewsc,3)=wx2
				views(viewsc,4)=wy2
				viewsset=TRUE
			End If
			flashoff=1
		Case 46'previous view
			If viewsset=FALSE And viewsc>0 Then
				showview
			Else
				If viewsi>1 Then
					viewsi=viewsi-1
					showview
				EndIf
			EndIf
			flashoff=1
		Case 47'next view
			If viewsset=FALSE And viewsc>0 Then
				showview
			Else
				If viewsi<viewsc Then
					viewsi=viewsi+1
					showview
				EndIf
			EndIf
			flashoff=1
		Case 48'first view
			If viewsset=FALSE And viewsc>0 Then
				viewsi=1
				showview
			Else
				If viewsc>0 And viewsi>1 Then
					viewsi=1
					showview
				EndIf
			EndIf
			flashoff=1
		Case 49'last view
			If viewsset=FALSE And viewsc>0 Then
				viewsi=viewsc
				showview
			Else
				If viewsc>0 And viewsi<viewsc Then
					viewsi=viewsc
					showview
				EndIf
			EndIf
			flashoff=1
		Case 50'delete view
			If viewsset=TRUE Then
				viewsset=FALSE
				'delete viewsi
				If viewsc=1 Then
					viewsc=0
					viewsi=0
				Else
					For i = viewsi To viewsc
						For j = 1 To 4
							views(i,j)=views((i+1),j)
						next
					Next
					viewsc=viewsc-1
					viewsi=viewsi-1
				EndIf
			EndIf
			flashoff=1
		Case 51
			tempint=selbutton
			If splining=TRUE Then
				splinemovingpointsx1=x1
				splinemovingpointsy1=y1
			EndIf
			for i = 1 to 10
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			for i = 11 to 20
				if buttonson(i)=true Then Exit for
			Next
			If i=21 Then
				selbutton=11'turn snap on
				enablebutton
			EndIf
			selbutton=tempint
		Case 55 To 57
			tempint=selbutton
			for i = 1 to 10
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			Select Case tempint
				Case 55
					selbutton=56
					turnbuttonoff
				Case 56
					selbutton=55
					turnbuttonoff
			End Select
			selbutton=tempint
			For i = 1 To 3
				If buttonson(57+i)=TRUE Then
					'ScreenSet 1,1
					
					Select Case selbutton
						Case 55,56
							para_offset=usersetoffsets(i)
						Case 57
							tempdouble=Val(inputbox("double","Set Offset","10"))
							usersetoffsets(i)=tempdouble
							flashoff=1
					End Select
					'screenset 0,0
					Exit For
				EndIf
			Next
			If i = 4 Then
				'ScreenSet 1,1
				
				Select Case selbutton
					Case 55,56
						tempdouble=Val(inputbox("double","Enter offset","10"))
						para_offset=tempdouble
						'screenset 0,0
						'flashoff=1
					Case 57
						'Locate 12,1
						'Print "Please FIRST select 1 of 3 user offsets. (buttons to the right)"
						'screenset 0,0
						flashoff=1
				End Select
			EndIf
		Case 58 To 60
			tempint=selbutton
			for i = 58 to 60
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			selbutton=tempint
			para_offset=usersetoffsets(selbutton-57)
		Case 61,62
			if groupexists=false then exit Sub
			tempint=selbutton
			for i = 1 to 20
				selbutton=i
				turnbuttonoff
			Next
			selbutton=122
			turnbuttonoff
			forcex=FALSE
			forcey=FALSE
			
			selbutton=11
			enablebutton
			snapenable=true
			selbutton=tempint
		Case 71 To 77
			If groupexists=true Then
				degroup
			End If
			tempint=selbutton
			for i = 1 to 10
				selbutton=i
				turnbuttonoff
			Next
			for i = 71 to 77
				selbutton=i
				turnbuttonoff
			Next
			selbutton=tempint
			'enablebutton
		Case 78
			'screenset 1,1
			dimprecision=Val(inputbox("double","Enter precision","10"))
			'screenset 0,0
			flashoff=1
		Case 79
			'screenset 1,1
			dimarrowsize=Val(inputbox("double","Enter arrow size","10"))
			'screenset 0,0
			flashoff=1
			
		Case 80
			'screenset 1,1
			dimleaderspacing=Val(inputbox("double","Enter leader offset","10"))
			'screenset 0,0
			flashoff=1
		Case 81 To 85
			If groupexists=true Then
				If linesingroupc>2 Then
					'find all common points
					'and for all common points found
					'then if useusersetcflength=TRUE then
					'chamfer/fillet all at once
					'else if free hand
					'then free hand them all at once
					proceed=FALSE
					For i = 86 To 90
						If buttonson(i)=TRUE Then
							proceed=TRUE
							Exit For
						EndIf
					Next
					If proceed=TRUE Then
						useusersetcflength=TRUE
						Select Case selbutton
							Case 81
								multichamferfillet("chamfer")
							Case 82
								multichamferfillet("fillet")
						End Select
						degroup
						inview()
						redraw
						Exit Sub
					End If
				EndIf
				'one idea is to be able to chamfer/fillet a sellected group
				'all at once and when i get this coded
				'i should degroup after finding all points to chamfer/fillet
				degroup
			End If
			Select Case selbutton
				Case 81 To 83
					tempint=selbutton
					for i = 1 to 10
						selbutton=i
						turnbuttonoff
					Next
					for i = 71 to 83'was 90 watch out for this change
						selbutton=i
						turnbuttonoff
					Next
					selbutton=tempint
					enablebutton
			End Select
			Select Case selbutton
				Case 81,82,84,85
					'81 Chamfer
					'82 Fillet
					'83 Best curve fit
					'84 free hand - use mouse tracking
					'85 set cflength
					'86,7,8,9,90 user settings
					'turnbuttonoff
					'drawlineatangle1 uses some validation
					Select Case selbutton
						Case 84'free hand - use mouse tracking
							useusersetcflength=FALSE
							enablebutton
						Case Else
							If buttonson(84)=FALSE Then
								For i = 1 To 5'86,7,8,9,90 user settings
									If buttonson(85+i)=TRUE Then
										'ScreenSet 1,1
										'locate 12,1
										Select Case selbutton
											Case 81'chamfer
												'Print "Chamfer @ length";useusersetcflengths(i)
												cflength=useusersetcflengths(i)
												'forcex=true
												'forcey=true
												useusersetcflength=TRUE
												'flashoff=1
											Case 82'fillet
												'Print "Fillet @ length";useusersetcflengths(i)
												cflength=useusersetcflengths(i)
												'forcex=true
												'forcey=true
												useusersetcflength=TRUE
												'flashoff=1
											Case 85'set cflength
												tempdouble=Val(inputbox("double","Set chamfer/fillet length","10"))
												useusersetcflengths(i)=tempdouble
												flashoff=1
												'ScreenSet 0,0
										End Select
										'screenset 0,0
										Exit For
									EndIf
								Next
								If i = 6 Then
									'ScreenSet 1,1
									
									Select Case selbutton
										Case 81,82
											
											tempdouble=Val(inputbox("double","Enter chamfer/fillet length","10"))
											useusersetcflengths(0)=tempdouble
											cflength=useusersetcflengths(0)
											'forcex=true
											'forcey=true
											useusersetcflength=TRUE
											'screenset 0,0
											'flashoff=1
										Case 85
											'Locate 12,1
											'Print "Please FIRST select 1 of 5 user chamfer/fillet lengths. (buttons to the right of the Set user chamfer/fillet lengths button)"
											'screenset 0,0
											flashoff=1
									End Select
									'If flashoff=0 Then screenset 0,0
								EndIf
							End If
					End Select
			End Select
		Case 86 To 90
			tempint=selbutton
			for i = 86 to 90
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				end if
			Next
			selbutton=tempint
			cflength=useusersetcflengths(selbutton-85)
		Case 91
			'screenset 1,1
			'tempstring=inputbox("string","Enter text","911 was an inside job")
			'screenset 0,0
			If tempstring<>"" Then plottextstring(UCase(tempstring),insertionx,insertiony,0)
			flashoff=1
		Case 93 To 97
			''screenset 1,1
			''locate 12,1
			'tempint=textsize
			''textsize=Val(inputbox("double","Enter new text size","10"))
			'If textsize=0 Then textsize=tempint
			''screenset 0,0
			'flashoff=1
			
			tempint=selbutton
			for i = 93 to 97
				if i<>tempint then
					selbutton=i
					turnbuttonoff
				Else
					Select Case i
						Case 93
							textsize=text_size_1
						Case 94
							textsize=text_size_2
						Case 95
							textsize=text_size_3
						Case 96
							textsize=text_size_4
						Case 97
							textsize=text_size_5
					End Select
				end if
			Next
			If textsize=0 Then textsize=22'maybe adjust size to fit text string in current view
			selbutton=tempint
			
			
		Case 101'scale
			If groupexists=false then exit Sub
			'screenset 1,1
			
			Dim As Double scale
			scale=Val(inputbox("double","Enter scale","2"))
			if scale=0 Then Exit Sub
			Dim As BOOLEAN scalingblock,scalingblocks(blockc)
			scalingblock=FALSE
			For i = 1 To linec
				If lines(i,8)=1 Then
					If lines(i,9)>0 Then
						scalingblock=TRUE
						scalingblocks(lines(i,9))=TRUE
					EndIf
					scaleit("LINE",i,scale,scale,scale,insertionx,insertiony)
				EndIf
			Next
			For i = 1 To circlec
				If circles(i,10)=1 Then
					If circles(i,12)>0 Then
						scalingblock=TRUE
						scalingblocks(circles(i,12))=TRUE
					EndIf
					scaleit("CIRCLE",i,scale,scale,scale,insertionx,insertiony)
				EndIf
			Next
			If scalingblock=TRUE Then
				For i = 1 To blockc
					If scalingblocks(i)=TRUE Then
						blockoffsets(i,1)=insertionx+(blockoffsets(i,1)-insertionx)*scale
						blockoffsets(i,2)=insertiony+(blockoffsets(i,2)-insertiony)*scale
						'blockoffsets(i,3)=
						blockoffsets(i,4)=blockoffsets(i,4)*scale
						blockoffsets(i,5)=blockoffsets(i,5)*scale
						blockoffsets(i,6)=0
					EndIf
				Next
			EndIf
			'screenset 0,0
			redraw
			flashoff=1
		Case 102
			'screenset 1,1
			
			tempint=scalefactor
			
			scalefactor=Val(inputbox("double","Enter new scale factor","2"))
			If scalefactor=0 Then scalefactor=tempint
			'screenset 0,0
			flashoff=1
		Case 103'scale UP
			Dim As BOOLEAN scalingblock,scalingblocks(blockc)
			scalingblock=FALSE
			If buttonson(105)=TRUE Then
				For i = 1 To linec
					If lines(i,8)=1 Then
						If lines(i,9)>0 Then
							scalingblock=TRUE
							scalingblocks(lines(i,9))=TRUE
						EndIf
						scaleit("LINE",i,scalexfactor,scaleyfactor,scalezfactor,insertionx,insertiony)
					EndIf
				Next
				For i = 1 To circlec
					If circles(i,10)=1 Then
						If circles(i,12)>0 Then
							scalingblock=TRUE
							scalingblocks(circles(i,12))=TRUE
						EndIf
						scaleit("CIRCLE",i,scalexfactor,scaleyfactor,scalezfactor,insertionx,insertiony)
					EndIf
				Next
				If scalingblock=TRUE Then
					For i = 1 To blockc
						If scalingblocks(i)=TRUE Then
							blockoffsets(i,1)=insertionx+(blockoffsets(i,1)-insertionx)*scalexfactor
							blockoffsets(i,2)=insertiony+(blockoffsets(i,2)-insertiony)*scaleyfactor
							'blockoffsets(i,3)=
							blockoffsets(i,4)=blockoffsets(i,4)*scalexfactor
							blockoffsets(i,5)=blockoffsets(i,5)*scaleyfactor
							blockoffsets(i,6)=0
						EndIf
					Next
				EndIf
			Else
				For i = 1 To linec
					If lines(i,8)=1 Then
						If lines(i,9)>0 Then
							scalingblock=TRUE
							scalingblocks(lines(i,9))=TRUE
						EndIf
						scaleit("LINE",i,scalexfactor,scaleyfactor,scalezfactor,insertionx,insertiony)
					EndIf
				Next
				For i = 1 To circlec
					If circles(i,10)=1 Then
						If circles(i,12)>0 Then
							scalingblock=TRUE
							scalingblocks(circles(i,12))=TRUE
						EndIf
						scaleit("CIRCLE",i,scalexfactor,scaleyfactor,scalezfactor,insertionx,insertiony)
					EndIf
				Next
				If scalingblock=TRUE Then
					For i = 1 To blockc
						If scalingblocks(i)=TRUE Then
							blockoffsets(i,1)=insertionx+(blockoffsets(i,1)-insertionx)*scalefactor
							blockoffsets(i,2)=insertiony+(blockoffsets(i,2)-insertiony)*scalefactor
							'blockoffsets(i,3)=
							blockoffsets(i,4)=blockoffsets(i,4)*scalefactor
							blockoffsets(i,5)=blockoffsets(i,5)*scalefactor
							blockoffsets(i,6)=0
						EndIf
					Next
				EndIf
			EndIf
			'screenset 0,0
			flashoff=1
			redraw
		Case 104'scale Down
			Dim As BOOLEAN scalingblock,scalingblocks(blockc)
			scalingblock=FALSE
			If buttonson(105)=TRUE Then
				For i = 1 To linec
					If lines(i,8)=1 Then
						If lines(i,9)>0 Then
							scalingblock=TRUE
							scalingblocks(lines(i,9))=TRUE
						EndIf
						scaleit("LINE",i,1/scalexfactor,1/scaleyfactor,1/scalezfactor,insertionx,insertiony)
					EndIf
				Next
				For i = 1 To circlec
					If circles(i,10)=1 Then
						If circles(i,12)>0 Then
							scalingblock=TRUE
							scalingblocks(circles(i,12))=TRUE
						EndIf
						scaleit("CIRCLE",i,1/scalexfactor,1/scaleyfactor,1/scalezfactor,insertionx,insertiony)
					EndIf
				Next
				If scalingblock=TRUE Then
					For i = 1 To blockc
						If scalingblocks(i)=TRUE Then
							blockoffsets(i,1)=insertionx+(blockoffsets(i,1)-insertionx)*1/scalexfactor
							blockoffsets(i,2)=insertiony+(blockoffsets(i,2)-insertiony)*1/scaleyfactor
							'blockoffsets(i,3)=
							blockoffsets(i,4)=blockoffsets(i,4)*1/scalexfactor
							blockoffsets(i,5)=blockoffsets(i,5)*1/scaleyfactor
							blockoffsets(i,6)=0
						EndIf
					Next
				EndIf
			Else
				For i = 1 To linec
					If lines(i,8)=1 Then
						If lines(i,9)>0 Then
							scalingblock=TRUE
							scalingblocks(lines(i,9))=TRUE
						EndIf
						scaleit("LINE",i,1/scalefactor,1/scalefactor,1/scalefactor,insertionx,insertiony)
					EndIf
				Next
				For i = 1 To circlec
					If circles(i,10)=1 Then
						If circles(i,12)>0 Then
							scalingblock=TRUE
							scalingblocks(circles(i,12))=TRUE
						EndIf
						scaleit("CIRCLE",i,1/scalefactor,1/scalefactor,1/scalefactor,insertionx,insertiony)
					EndIf
				Next
				If scalingblock=TRUE Then
					For i = 1 To blockc
						If scalingblocks(i)=TRUE Then
							blockoffsets(i,1)=insertionx+(blockoffsets(i,1)-insertionx)*1/scalefactor
							blockoffsets(i,2)=insertiony+(blockoffsets(i,2)-insertiony)*1/scalefactor
							'blockoffsets(i,3)=
							blockoffsets(i,4)=blockoffsets(i,4)*1/scalefactor
							blockoffsets(i,5)=blockoffsets(i,5)*1/scalefactor
							blockoffsets(i,6)=0
						EndIf
					Next
				EndIf
			EndIf
			'screenset 0,0
			flashoff=1
			redraw
		Case 105
			'do i need to do anything when turn Indivudal scaling on
		Case 106
			'screenset 1,1
			
			tempint=scalexfactor
			
			scalexfactor=Val(inputbox("double","Enter new scale X factor","2"))
			If scalexfactor=0 Then scalexfactor=tempint
			'screenset 0,0
			flashoff=1
		Case 107
			'screenset 1,1
			
			tempint=scaleyfactor
			
			scaleyfactor=Val(inputbox("double","Enter new scale Y factor","2"))
			If scaleyfactor=0 Then scaleyfactor=tempint
			'screenset 0,0
			flashoff=1
		Case 108
			'screenset 1,1
			
			tempint=scalezfactor
			
			scalezfactor=Val(inputbox("double","Enter new scale Z factor","2"))
			If scalezfactor=0 Then scalezfactor=tempint
			'screenset 0,0
			flashoff=1
		Case 109'scale with grab handles
			if groupexists=false Then Exit Sub
			enablebutton
			redraw
			flashoff=1
		Case 111'block
			theboxbelow("create block "+blockname)
			if groupexists=false Then Exit Sub
			'because nested blocks is a huge issue
			'explode any blocks in group first then proceede
			explode_block
			
			If blockc>0 Then
				If blocklevelc>0 Then
					ReDim tempintarray(blockc,blocklevelc+1)
					For i = 1 To blockc
						For j=1 To blocklevelc+1
							tempintarray(i,j)=blocklevel(i,j)
						Next
					Next
					ReDim blocklevel(blockc+2,blocklevelc+2)
					For i = 1 To blockc
						For j=1 To blocklevelc+1
							blocklevel(i,j)=tempintarray(i,j)
						Next
					Next
				Else
					'ReDim blocklevel(blockc+2,blocklevelc+2)
					ReDim blocklevel(blockc+2,2)
				End If
			Else
				'ReDim blocklevel(blockc+2,blocklevelc+2)
				ReDim blocklevel(2,2)
			End If
			Dim blcincset As BOOLEAN
			blcincset=FALSE
			blockc=blockc+2
			ReDim Preserve blocknames(blockc)
			ReDim Preserve blockoffsets(blockc,7)
			ReDim Preserve blockstatus(blockc)
			Dim As Integer bibi,blocknumber,toplevelblocknumber
			Dim As BOOLEAN blocksinbox(blockc)
			For i = 1 To blockc
				blocksinbox(i)=FALSE
			Next
			
			
			blockc=blockc-1
			'figure out the extents for this block
			'in order to to set blockoffsets
			Dim As BOOLEAN blockextentsset
			Dim As Integer blockentityc
			blockextentsset=FALSE
			blockentityc=0
			'savedxfblockonly=TRUE
			'savedxfblockonlyi=blockc
			For i = 1 To linec
				If lines(i,8)=1 Then
					If lines(i,9)>0 Then 'this line is going to be in a nested block
						If blcincset=FALSE Then
							blcincset=TRUE
							blocklevelc=blocklevelc+1
						EndIf
						lines(i,8)=0
						blocksinbox(lines(i,9))=TRUE
						For j = 1 To blocklevelc
							If blocklevel(lines(i,9),j+1)=blockc+1 Then Exit For
						Next
						If j=blocklevelc+1 Then
							blocklevel(lines(i,9),1)=blocklevel(lines(i,9),1)+1
							blocklevel(lines(i,9),blocklevel(lines(i,9),1)+1)=blockc+1
						End If
					Else
						lines(i,9)=blockc
					EndIf
					'lines(i,9)=blockc
					'blockentityc is not even used for anything???
					blockentityc=blockentityc+1
					If blockextentsset=FALSE Then
						blockextentsset=TRUE
						extentsx1=lines(i,1)
						extentsy1=lines(i,2)
						extentsx2=lines(i,1)
						extentsy2=lines(i,2)
						If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)
						If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)
						If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)
						If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)
					Else
						If lines(i,1)<extentsx1 Then extentsx1=lines(i,1)
						If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)
						If lines(i,2)<extentsy1 Then extentsy1=lines(i,2)
						If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)
						If lines(i,1)>extentsx2 Then extentsx2=lines(i,1)
						If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)
						If lines(i,2)>extentsy2 Then extentsy2=lines(i,2)
						If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)
					EndIf
				EndIf
			Next
			For i = 1 To circlec
				If circles(i,10)=1 Then
					If circles(i,12)>0 Then 'this line is going to be in a nested block
						If blcincset=FALSE Then
							blcincset=TRUE
							blocklevelc=blocklevelc+1
						EndIf
						circles(i,10)=0
						blocksinbox(circles(i,12))=true
						For j = 1 To blocklevelc
							If blocklevel(circles(i,12),j+1)=blockc+1 Then Exit For
						Next
						If j=blocklevelc+1 Then
							blocklevel(circles(i,12),1)=blocklevel(circles(i,12),1)+1
							blocklevel(circles(i,12),blocklevel(circles(i,12),1)+1)=blockc+1
						End If
					Else
						circles(i,12)=blockc
					EndIf

					'circles(i,12)=blockc
					blockentityc=blockentityc+1
					If blockextentsset=FALSE Then
						blockextentsset=TRUE
						extentsx1=circles(i,1)-circles(i,4)
						extentsx2=circles(i,1)+circles(i,4)
						If circles(i,1)-circles(i,8)<extentsx1 Then extentsx1=circles(i,1)-circles(i,8)
						If circles(i,1)+circles(i,8)>extentsx2 Then extentsx2=circles(i,1)+circles(i,8)
						extentsy1=circles(i,2)-circles(i,4)
						extentsy2=circles(i,2)+circles(i,4)
						If circles(i,2)-circles(i,8)<extentsy1 Then extentsy1=circles(i,2)-circles(i,8)
						If circles(i,2)+circles(i,8)>extentsy2 Then extentsy2=circles(i,2)+circles(i,8)
					Else
						If circles(i,1)-circles(i,4)<extentsx1 Then extentsx1=circles(i,1)-circles(i,4)
						If circles(i,1)-circles(i,8)<extentsx1 Then extentsx1=circles(i,1)-circles(i,8)
						If circles(i,2)-circles(i,4)<extentsy1 Then extentsy1=circles(i,2)-circles(i,4)
						If circles(i,2)-circles(i,8)<extentsy1 Then extentsy1=circles(i,2)-circles(i,8)
						If circles(i,1)+circles(i,4)>extentsx2 Then extentsx2=circles(i,1)+circles(i,4)
						If circles(i,1)+circles(i,8)>extentsx2 Then extentsx2=circles(i,1)+circles(i,8)
						If circles(i,2)+circles(i,4)>extentsy2 Then extentsy2=circles(i,2)+circles(i,4)
						If circles(i,2)+circles(i,8)>extentsy2 Then extentsy2=circles(i,2)+circles(i,8)
					End If
				End If
			Next
			'what about setting the base point of the block???
			'is it insertionx and insertiony that is used to define base point
			If buttonson(113)=TRUE Then'block insertion point
				theboxbelow("block insertion points "+Str(insertionx)+" "+Str(insertiony))
				blockoffsets(blockc,1)=insertionx
				blockoffsets(blockc,2)=insertiony
				blockoffsets(blockc,3)=0
				extentsx1=extentsx1-insertionx
				extentsy1=extentsy1-insertiony
				extentsx2=extentsx2-insertionx
				extentsy2=extentsy2-insertiony
				buttonson(113)=FALSE
			Else
				blockoffsets(blockc,1)=extentsx1
				blockoffsets(blockc,2)=extentsy1
				blockoffsets(blockc,3)=0
				extentsx2=extentsx2-extentsx1
				extentsy2=extentsy2-extentsy1
				extentsx1=0
				extentsy1=0
			EndIf
			modifyx1=blockoffsets(blockc,1)
			modifyy1=blockoffsets(blockc,2)
			modifyx2=0
			modifyy2=0
			movedxfblock(blockc)
			'save block dxf file
			'if file name exist then save as filenamecopy(x).dxf
			'(blockc,7)
			'1=insertionx,2=insertiony,3=insertionz
			'4=scalex,5=scaley,6=scalez
			'7=rotation
			savedxfblockonly=TRUE
			savedxfblockonlyi=blockc
			blocknames(blockc)=blockname
			insertionx=blockoffsets(blockc,1)
			insertiony=blockoffsets(blockc,2)
			blockoffsets(blockc,1)=0
			blockoffsets(blockc,2)=0
			blockoffsets(blockc,3)=0
			blockoffsets(blockc,4)=1
			blockoffsets(blockc,5)=1
			blockoffsets(blockc,6)=1
			blockoffsets(blockc,7)=0
			'save just this block data in a dxf file
			'savedxf("drawing.dxf") 'no need to specify name in quotes
			savedxf("")'this should be used for wblock
			savedxfblockonly=FALSE
			'blockstatus is set
			'true for master blocks
			'false for coppies
			blockstatus(blockc)=-1
			
			'i think these two fors were part of the idea of testing below
			For i = 1 To linec
				If lines(i,8)=1 Then lines(i,8)=-1
			Next
			For i = 1 To circlec
				If circles(i,10)=1 Then circles(i,10)=-1
			Next
			
'maybe i was testing for importing block from file
'something to do with blockstatus=-2
			'for testing purposes
			'no i think it's only for testing purposes
			'maybe when i was coding it originally
			'but now, without using the code below, it fails
			'delete the block (the two for next loops above)
			'import it back into the drawing at the insertion
			'point of blockoffsets
			i=blockc
			blockc=blockc+1
			tempint=linec
			For j=1 To tempint
				If lines(j,9)=i Then
					memmanageline
					For k=1 To 9
						lines(linec,k)=lines(j,k)
					Next
					lines(linec,8)=1
					lines(linec,9)=blockc
					'For k = 0 To linesblocklevelc'(j,0)
					'	linesblocklevel(linec,k)=linesblocklevel(j,k)
					'Next
				End If
			Next
			tempint=circlec
			For j=1 To tempint
				If circles(j,12)=i Then
					memmanagecircle
					For k=1 To 12
						circles(circlec,k)=circles(j,k)
					Next
					circles(circlec,10)=1
					circles(circlec,12)=blockc
					'For k = 0 To circlesblocklevelc'(j,0)
					'	circlesblocklevel(circlec,k)=circlesblocklevel(j,k)
					'Next
				EndIf
			Next

			modifyx1=0
			modifyy1=0
			modifyx2=insertionx
			modifyy2=insertiony
			movedxfblock(blockc)
			blocknames(blockc)=blockname
			blockoffsets(blockc,1)=insertionx
			blockoffsets(blockc,2)=insertiony
			blockoffsets(blockc,3)=0
			blockoffsets(blockc,4)=1
			blockoffsets(blockc,5)=1
			blockoffsets(blockc,6)=1
			blockoffsets(blockc,7)=0
			
			If blocklevelc>0 Then
				For bibi=1 To blockc
					If blocksinbox(bibi)=TRUE Then
						blocknumber=bibi
						If blocklevel(blocknumber,1)=0 Then
							toplevelblocknumber=blocknumber
							blocksinbox(blocknumber)=TRUE
						Else
							toplevelblocknumber=blocklevel(blocknumber,blocklevel(blocknumber,1)+1)
							blocksinbox(toplevelblocknumber)=TRUE
						EndIf
						For i=1 To blockc
							If blocklevel(i,blocklevel(i,1)+1)=toplevelblocknumber Then
								blocksinbox(i)=TRUE
							EndIf
						Next
					EndIf
				Next
			End If
			For i = 1 To linec
				If lines(i,9)>0 And lines(i,8)<>-1 Then
					If blocksinbox(lines(i,9))=TRUE Then lines(i,8)=1
				End If
			Next
			For i = 1 To circlec
				If circles(i,12)>0 And circles(i,10)<>-1 Then
					If blocksinbox(circles(i,12))=TRUE Then circles(i,10)=1
				End If
			Next
			
			
			'screenset 0,0
			
			flashoff=1
			inview
			mousexp=mousex-1
			tempmousex=mousexp-1
			'redraw
			''showgroups
		Case 112'insert block
			tempint=selbutton
			for i = 1 to 10
				selbutton=i
				turnbuttonoff
			Next
			selbutton=tempint
			Dim As Integer loadedblockn=0
			'ScreenSet 1,1
			'i thought i was using open dialog gui for this
			'blockname=inputbox("string","Enter Block Name","")
			For i = 1 To blockc
				If blockstatus(i)=-1 And blocknames(i)=blockname Then
					loadedblockn=i
					Exit For
				End If
			Next
			'check if valid file name
			degroup
			redraw
			If loadedblockn=0 Then
				tempstring=blockname+".dxf"
				theboxbelow(tempstring)
				'need to see if block has already been loaded
				'befor i load it twice here
				If Dir$(tempstring)="" Then
					'open dialog
					'ff=FreeFile
					'Open "mygui_open_save_saveas_task.txt" For Output As #ff
					'Print #ff, "open"
					'Close #ff
					'Chain "mygui.exe"
					'Open "mygui_open_save_file_name.txt" For Input As #ff
					'Line Input #ff,tempstring
					'Close #ff
					If tempstring<>"null" Then
						'newdrawing
						If LCase(Right(tempstring,4))=".dxf" Then
							blockname=Mid(tempstring,1,Len(tempstring)-4)
							importblock
						EndIf
					End If
				Else
					importblock
				EndIf
			Else
				copybaseblock(loadedblockn)
			EndIf
			'ScreenSet 0,0
			flashoff=1
			inview()
			'showgroups
			If buttonson(113)=FALSE Then
				modifying=TRUE
				modify=31
			End If
		Case 113'block insertion point
			'with this button on - inserted blocks are positioned at insertionxy
		Case 114'explode block
			if groupexists=false then exit Sub
			flashoff=1
			explode_block
			degroup
			redraw
		Case 116'edit block
			'save the current drawing as temp file name unless it already has a file name
			If openeddrawingname="" Then openeddrawingname="temp0001.dxf"
			edit_master_block_from_dxf_file_name=openeddrawingname
			savedxf(openeddrawingname)
			'save current view of current drawing
			If viewsset=FALSE Then
				memmanageviews
				viewsi=viewsc
				views(viewsc,1)=wx1
				views(viewsc,2)=wy1
				views(viewsc,3)=wx2
				views(viewsc,4)=wy2
				viewsset=TRUE
			End If
			escapeme
			escapeall
			newdrawing
			openeddrawingname=edit_master_block_from_dxf_file_name
			importdxf
			countactiveobjects
			For i=1 To blockc
				If blocknames(i)=edit_master_block_name Then
					edit_master_block_number=i
					Exit For
				EndIf
			Next
			edit_master_block=TRUE
			'edit_blocks_inserts_c,edit_blocks_inserts()
			edit_blocks_inserts_c=0
			For i=edit_master_block_number+1 To blockc
				If blocknames(i)=blocknames(edit_master_block_number) Then edit_blocks_inserts_c+=1
			Next
			ReDim edit_blocks_inserts(edit_blocks_inserts_c)
			c=0
			For i=edit_master_block_number+1 To blockc
				If blocknames(i)=blocknames(edit_master_block_number) Then
					c+=1
					edit_blocks_inserts(c)=i
				EndIf
			Next
			
			'open the master block drawing
			escapeme
			escapeall
			newdrawing
			openeddrawingname=edit_master_block_name+".dxf"
			importdxf
			countactiveobjects
			'save master block to master block name _old.dxf for backup (undo) purposes
			openeddrawingname=edit_master_block_name+"_old.dxf"
			savedxf(openeddrawingname)
			're-open the master block drawing
			escapeme
			escapeall
			newdrawing
			openeddrawingname=edit_master_block_name+".dxf"
			importdxf
			countactiveobjects
			gtk_window_set_title(GTK_WINDOW(win), My_GTK_APP_name + "   " + openeddrawingname)
			'disable all block buttons while master block is open
			disable_all_block_buttons=TRUE
			'let the user modify and save the master block drawing
			'becuase edit_master_block=TRUE when user clicks the save button
			'toolbar_button_save_op has in if edit_master_block=TRUE then at the bottom
			'which finishes the work of editing a master block
		Case 121
			flashoff=1
			enablebutton
			redraw
		Case 122
			snapenable=true
		Case 123
			'screenset 1,1
			
			gridxspacing=Val(inputbox("double","Enter grid X spacing",""))
			'ScreenSet 0,0
			redraw
			flashoff=1
		Case 124
			'screenset 1,1
			gridyspacing=Val(inputbox("double","Enter grid Y spacing",""))
			'screenset 0,0
			redraw
			flashoff=1
		Case 125
			'screenset 1,1
			gridxoffset=Val(inputbox("double","Enter grid X offset",""))
			'screenset 0,0
			redraw
			flashoff=1
		Case 126
			'screenset 1,1
			gridyoffset=Val(inputbox("double","Enter grid Y offset",""))
			'screenset 0,0
			redraw
			flashoff=1
		Case 131
			
		Case 132
			tempdouble=fbcadupm
			'screenset 1,1
			fbcadupm=Val(inputbox("double","Enter Units per meter",""))
			If fbcadupm=0 Then fbcadupm=tempdouble
			'screenset 0,0
			redraw
			flashoff=1
		Case 133
			tempdouble=gravity
			'ScreenSet 1,1
			gravity=Val(inputbox("double","Enter Gravity",""))
			If gravity=0 Then gravity=tempdouble
			'screenset 0,0
			redraw
			flashoff=1
		Case 134
			tempdouble=trajiv
			'ScreenSet 1,1
			trajiv=Val(inputbox("double","Enter Initial Velocity",""))
			If trajiv=0 Then trajiv=tempdouble
			'ScreenSet 0,0
			redraw
			flashoff=1
		Case 135
			tempdouble=trajtheta
			'ScreenSet 1,1
			trajtheta=Val(inputbox("double","Enter Theta",""))
			If trajtheta=0 Then trajtheta=tempdouble
			'ScreenSet 0,0
			redraw
			flashoff=1
		'Case 136
		'	ScreenSet 1,1
		'	
		'	tempdouble=trajix
		'	locate 12,1
		'	Input "Enter starting point X ";trajix
		'	If trajix=0 Then trajix=tempdouble
		'	
		'	tempdouble=trajiy
		'	locate 12,1
		'	Input "Enter starting point Y ";trajiy
		'	If trajiy=0 Then trajiy=tempdouble
		'	
		'	tempdouble=trajiz
		'	locate 12,1
		'	Input "Enter starting point Z ";trajiz
		'	If trajiz=0 Then trajiz=tempdouble
			
			'ScreenSet 0,0
			redraw
			flashoff=1

			'	gravity=9.81
'	fbcadupm=1
'	trajiv=0
'	trajtheta=0
'	trajix=0
'	trajiy=0
'	trajiz=0
'Data 131,"T","off","Trajectory Snap","Snaps to the point of impact of a tracectory based on tractectory settings"
'Data 132,"U","off","Units per meter","Set the number of fbcad units that represent one meter"
'Data 133,"G","off","Gravity","Set Gravity default is 9.81"
'Data 134,"V","off","Velocity","Set initial velocity"
'Data 135,"t","off","Theta","Set theta"
'Data 136,"I","off","Initial point","Select Initial starting point of tracjectory"
		Case 141 'gcode
			If groupexists=true Then
				'create gcode for all entites
				glines_group=TRUE
			Else
				'created gcode for selected entities
				glines_group=FALSE
			End If
			gcode_create_gcode
			
			'tempint=selbutton
			'for i = 1 to 10
			'	selbutton=i
			'	turnbuttonoff
			'Next
			''for i = 31 to 140
			''	selbutton=i
			''	turnbuttonoff
			''Next
			'selbutton=tempint
			'gff=FreeFile
			'Open "gcodeout.txt" For Output As #gff
			'Close #gff
			'theboxbelow("new gcodeout file started"+crlf)
		Case 142
			flashoff=1
		Case 143
			flashoff=1
		Case 144
			flashoff=1
	end Select
	If flashoff=0 Then enablebutton
End sub
Sub explode_block()
	Dim As Integer i,j,k,flashoff
	Dim As Integer selectedblockn(blockc)
	For i = 1 To linec
		If lines(i,8)=1 And lines(i,9)>0 Then selectedblockn(lines(i,9))=TRUE
	Next
	For i = 1 To circlec
		If circles(i,10)=1 And circles(i,12)>0 Then selectedblockn(circles(i,12))=TRUE
	Next
	For i = 1 To linec
		For j=1 To blockc
			If lines(i,9)=j And selectedblockn(j)=TRUE And lines(i,8)<>1 Then selectedblockn(j)=FALSE
		Next
	Next
	For i = 1 To circlec
		For j=1 To blockc
			If circles(i,12)=j And selectedblockn(j)=TRUE And circles(i,10)<>1 Then selectedblockn(j)=FALSE
		Next
	Next
	Dim As BOOLEAN explodeit,explodeall,stopexploding
	For i=1 To blockc
		If selectedblockn(i)=TRUE Then
			'set explodeall to false if user preferance is set to prompt prior to exploding
			'user preferance settings is still pending (to be added to menu)
			explodeall=TRUE
			explodeit=FALSE
			stopexploding=FALSE
			Exit For
		EndIf
	Next
	For i=1 To blockc
		If selectedblockn(i)=TRUE Then
			'i need to hi-lite each block so user knows which one is
			'in question
			If explodeall=FALSE Then
				'this prompt is to be displayed where?
				'in a msg box ?
				'in the box below ?
				tempstring="explode block "+blocknames(i)+" y/n/a/(esc) "
				'Print tempstring;
				Do While TRUE
					Sleep 1
					Select Case InKey$
						Case "y","Y"
							'Print "Yes"
							explodeit=TRUE
							Exit Do
						Case "n","N"
							'Print "No"
							explodeit=FALSE
							Exit Do
						Case "a","A"
							explodeall=TRUE
							Exit Do
						Case Chr(27)
							stopexploding=TRUE
							Exit Do
					End Select
				Loop
			End If
			If stopexploding=TRUE Then Exit For
			If explodeall=TRUE Or explodeit=TRUE Then
				'if this block (i) is a top levle block then
				If blocklevelc>0 Then
					If blocklevel(i,1)=0 Then
						blockstatus(i)=-2
						theboxbelow("Exploding block "+blocknames(i)+crlf)
						For j=1 To linec
							If lines(j,9)=i Then lines(j,9)=0
						Next
						For j=1 To circlec
							If circles(j,12)=i Then circles(j,12)=0
						Next
					Else
						blocklevel(i,blocklevel(i,1)+1)=0
						blocklevel(i,1)=blocklevel(i,1)-1
					End If
				Else
					blockstatus(i)=-2
					theboxbelow("Exploding block "+blocknames(i)+crlf)
					For j=1 To linec
						If lines(j,9)=i Then lines(j,9)=0
					Next
					For j=1 To circlec
						If circles(j,12)=i Then circles(j,12)=0
					Next
				EndIf
				'theboxbelow("Exploding block "+blocknames(i)+crlf)
				'blockstatus(i)=-2
				''Dim As Byte blockstillactive
				''blockstillactive=FALSE
				'For j=1 To linec
				'	'If lines(j,9)=i Then
				'	'	If linesblocklevel(j,0)>0 Then
				'	'		linesblocklevel(j,linesblocklevel(j,0))=0
				'	'		linesblocklevel(j,0)=linesblocklevel(j,0)-1
				'	'	EndIf
				'	'	If linesblocklevel(j,0)=0 Then
				'	'		lines(j,9)=0
				'	'	Else
				'	'		blockstillactive=TRUE
				'	'	End If
				'	'End If
				'	If lines(j,9)=i Then lines(j,9)=0
				'Next
				'For j=1 To circlec
				'	'If circles(j,12)=i Then
				'	'	If circlesblocklevel(j,0)>0 Then
				'	'		circlesblocklevel(j,circlesblocklevel(j,0))=0
				'	'		circlesblocklevel(j,0)=circlesblocklevel(j,0)-1
				'	'	EndIf
				'	'	If circlesblocklevel(j,0)=0 Then
				'	'		circles(j,12)=0
				'	'	Else
				'	'		blockstillactive=TRUE
				'	'	EndIf
				'	'EndIf
				'	If circles(j,12)=i Then circles(j,12)=0
				'Next
				''If blockstillactive=FALSE Then blockstatus(i)=-2
			End If
		EndIf
	Next
End Sub
Sub enablebutton
	buttonson(selbutton)=TRUE
	turn_gtk_button_on
End Sub
Sub orthomode()
	if ortho=true then ortho=false else ortho=TRUE
	'if ortho=true and buttonson(selbutton)=false then
	'	selbutton=21
	'	turnbuttonon
	'end If
	'if ortho=true and buttonson(22)=false then
	'	angle2=0
	'end If
	If ortho=TRUE and buttonson(22)=FALSE Then
		'screenset 0,0:view:Window
		'mousexp=mousex-1
		'tempmousex=mousexp-1
		'selbutton=22
		'turnbuttonon
		selbutton=21
	Else
		If buttonson(22)=FALSE Then
			drawatangle=FALSE
			forcex=FALSE
			forcey=FALSE
		EndIf
	end If
end sub
Sub escapeall()
	Dim i As Integer
	mouse_clicks=0
	drawing=false
	drawmode=FALSE
	arcstarted=FALSE
	arcing=FALSE
	groupscaling=FALSE
	groupgrab=FALSE
	forcelength=FALSE
	drawatangle=false
	forcex=FALSE
	forcey=FALSE
	forcedx=FALSE
	forcedy=FALSE
	ellipsing=false
	ellipsstarted=false
	ellipsarcing=FALSE
	createcircleset=FALSE
	splinestarted=FALSE
	splining=FALSE
	spliningmovingpoints=FALSE
	setsplinedown=FALSE
	
	'If rayenabled=TRUE Then
	'	rayenabled=FALSE
	'	For i = rayi To rayi+7
	'		lines(i,8)=-1
	'	Next
	'	'inview
	'	'redraw
	'End If
End Sub
sub setxyz()
	'here is where multiple entries are needed from my input gui
	mousexp=mousex-1
	tempmousex=mousexp-1
	usemyguicalc=FALSE
	Select Case drawing
		Case TRUE
			If selentity=TRUE Then
				If absrel=TRUE Then
					tempstring=inputbox("double","Enter value for X2",Str(fxm))
					x2=Val(tempstring)
				Else
					tempstring=inputbox("double","Enter relative value for X2",Str(fxm))
					x2=fxm+Val(tempstring)
				End If
				If tempstring<>""  Then
					If absrel=TRUE Then
						tempstring=inputbox("double","Enter value for Y2",Str(fym))
						y2=val(tempstring)
					Else
						tempstring=inputbox("double","Enter relative value for Y2",Str(fym))
						y2=fym+val(tempstring)
					End If
					If tempstring<>"" Then
						createline
					End If
				End If
			Else
				tempstring=inputbox("double","Enter value for X2","")
				If tempstring<>""  Then
					x2=val(tempstring)
					tempstring=inputbox("double","Enter value for Y2","")
					If tempstring<>"" Then
						y2=val(tempstring)
						'drawmode=TRUE
						createline
					End If
				End If
			EndIf
		Case FALSE
			If selentity=TRUE Then
				tempstring=inputbox("double","Enter value for X1",Str(fxm))
				If tempstring<>""  Then
					x1=val(tempstring)
					tempstring=inputbox("double","Enter value for Y1",Str(fym))
					If tempstring<>"" Then
						y1=val(tempstring)
						drawmode=TRUE
					End If
				End If
			Else
				tempstring=inputbox("double","Enter value for X1","")
				If tempstring<>""  Then
					x1=val(tempstring)
					tempstring=inputbox("double","Enter value for Y1","")
					If tempstring<>"" Then
						y1=val(tempstring)
						drawmode=TRUE
					End If
				End If
			EndIf
	End Select
	usemyguicalc=TRUE


'	Dim valuestring As string
'	locate 12,1
'	print "X=         ";
'	tempstring=""
'	valuestring=""
'	do while true
'		tempstring=inkey$
'		select case tempstring
'			case chr$(13)
'				exit Do
'			Case Chr$(27)
'				Exit sub
'			case else
'				valuestring=valuestring+tempstring
'				locate 12,3
'				print valuestring;" ";
'				tempstring=""
'		end select
'	Loop
'	if mid$(valuestring,1,1)="@" then
'		'this will be relative to point
'	Else
'		If valuestring<>"" then
'			if drawmode=true Then
'			 	forcex=true
'				fx=Val(valuestring)
'			Else
'				x1=Val(valuestring)
'			EndIf
'		End if
'	end If
'
'
'
'	locate 12,1
'	print "Y=         ";
'	tempstring=""
'	valuestring=""
'	do while true
'		tempstring=inkey$
'		select case tempstring
'			case chr$(13)
'				exit Do
'			Case Chr$(27)
'				Exit Sub
'			case else
'				valuestring=valuestring+tempstring
'				locate 12,3
'				print valuestring;" ";
'				tempstring=""
'		end select
'	loop
'	if mid$(valuestring,1,1)="@" then
'		'this will be relative to point
'	Else
'		If valuestring<>"" then
'			if drawmode=true Then
'			 	forcey=true
'				fy=Val(valuestring)
'			Else
'				y1=Val(valuestring)
'			EndIf
'		End if
'	end If
'
'
''	locate 12,1
''	print "Z=         ";
''	tempstring=""
''	valuestring=""
''	do while true
''		tempstring=inkey$
''		select case tempstring
''			case chr$(13)
''				exit Do
''			Case Chr$(27)
''				Exit Sub
''			case else
''				valuestring=valuestring+tempstring
''				locate 12,3
''				print valuestring;" ";
''				tempstring=""
''		end select
''	loop
''	if mid$(valuestring,1,1)="@" then
''		'this will be relative to point
''	else
''		If valuestring<>"" then
''			if drawmode=true Then
''			 	forcez=true
''				fz=Val(valuestring)
''			Else
''				z1=Val(valuestring)
''			EndIf
''		End if
''	end If
'	drawmode=true
End sub
Sub setx1()
	mousexp=mousex-1
	tempmousex=mousexp-1
	usemyguicalc=FALSE
	If selentity=TRUE Then
		tempstring=inputbox("double","Enter value for X1",Str(fxm))
	Else
		tempstring=inputbox("double","Enter value for X1",Str(damx))
	EndIf
	If tempstring<>""  Then
		fx=val(tempstring)
		forcex=TRUE
		If forcey=TRUE Then
			x1=fx
			y1=fy
			drawmode=TRUE
			forcex=FALSE
			forcey=FALSE
		EndIf
	End If
End Sub
Sub sety1()
	mousexp=mousex-1
	tempmousex=mousexp-1
	usemyguicalc=FALSE
	If selentity=TRUE Then
		tempstring=inputbox("double","Enter value for Y1",Str(fym))
	Else
		tempstring=inputbox("double","Enter value for Y1",Str(damy))
	EndIf
	If tempstring<>""  Then
		fy=val(tempstring)
		forcey=TRUE
		If forcex=TRUE Then
			x1=fx
			y1=fy
			drawmode=TRUE
			forcex=FALSE
			forcey=FALSE
		EndIf
	End If
End Sub
Sub setx2()
	mousexp=mousex-1
	tempmousex=mousexp-1
	usemyguicalc=FALSE
	If selentity=TRUE Then
		tempstring=inputbox("double","Enter value for X2",Str(fxm))
	Else
		tempstring=inputbox("double","Enter value for X2",Str(damx))
	EndIf
	If tempstring<>""  Then
		fx=val(tempstring)
		forcex=TRUE
		forcedx=TRUE
		fxd=fx
		If forcey=TRUE Then
			createline
		EndIf
	End If
End Sub
Sub sety2()
	mousexp=mousex-1
	tempmousex=mousexp-1
	usemyguicalc=FALSE
	If selentity=TRUE Then
		tempstring=inputbox("double","Enter value for Y2",Str(fym))
	Else
		tempstring=inputbox("double","Enter value for Y2",Str(damy))
	EndIf
	If tempstring<>""  Then
		fy=val(tempstring)
		forcey=TRUE
		forcedy=TRUE
		fyd=fy
		If forcex=TRUE Then
			createline
		EndIf
	End If
End Sub
sub drawlineatangle1()
	'ScreenSet 0,0:view:Window
	selbutton=22
	mousexp=mousex-1
	tempmousex=mousexp-1
	If drawatangle=TRUE Then
		drawatangle=FALSE
		forcex=false
		forcey=FALSE
		turnbuttonoff
		Exit Sub
	EndIf
	If selentity=TRUE And otd="line" Then
		newanglestring=inputbox("double","New angle",Str(selangle))
	Else
		newanglestring=inputbox("double","New angle",Str(angle))
	EndIf
	'newanglestring=inputbox("double","New angle",Str(selangle))
	'if Mid$(newanglestring,1,1)="@" then
	'	angle2=val(mid$(newanglestring,2))+selangle
	'else
	'	angle2=val(newanglestring)
	'end if
	If newanglestring<>"" Then
		angle2=val(newanglestring)
		forcex=true
		forcey=true
		drawatangle=TRUE
		enablebutton
	End If
end Sub
Sub drawlineatangle2()
	'ScreenSet 0,0:view:Window
	mousexp=mousex-1
	tempmousex=mousexp-1
	selbutton=22
	If drawatangle=TRUE Then
		drawatangle=FALSE
		forcex=false
		forcey=FALSE
		turnbuttonoff
		Exit Sub
	EndIf
	angle2=angle
	forcex=TRUE
	forcey=TRUE
	drawatangle=TRUE
	enablebutton
end sub
sub movegroup()
	Dim As Integer i
	for i = 1 to linec
		if lines(i,8)>0 Then
			line (lines(i,1)+(modifyx2-modifyx1),lines(i,2)+(modifyy2-modifyy1))-(lines(i,4)+(modifyx2-modifyx1),lines(i,5)+(modifyy2-modifyy1)),lines(i,7)
		end if
	Next
	For i = 1 To circlec
		If circles(i,10)>0 Then
			Select Case circles(i,9)
				Case 1
					circle(circles(i,1)+(modifyx2-modifyx1),circles(i,2)+(modifyy2-modifyy1)),circles(i,4),circles(i,5)
				Case 2
					circle(circles(i,1)+(modifyx2-modifyx1),circles(i,2)+(modifyy2-modifyy1)),circles(i,4),circles(i,5),circles(i,6),circles(i,7)
				Case 3,4
					plotellipse(circles(i,1)+(modifyx2-modifyx1),circles(i,2)+(modifyy2-modifyy1),circles(i,3),circles(i,4),35,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
			End Select
		EndIf
	Next
end sub
sub setmovedown()
	theboxbelow("setmovedown")
	Dim As Integer i
	Dim As BOOLEAN movingblock,movingblocks(blockc)
	'Dim As Integer movingblocks(blockc)
	movingblock=FALSE
	for i = 1 to linec
		If lines(i,8)=2 Then lines(i,8)=1
		if lines(i,8)>0 Then
			If lines(i,9)>0 Then
				movingblock=TRUE
				movingblocks(lines(i,9))=TRUE
			EndIf
			lines(i,1)=lines(i,1)+(modifyx2-modifyx1)
			lines(i,2)=lines(i,2)+(modifyy2-modifyy1)
			lines(i,3)=lines(i,3)+(modifyz2-modifyz1)
			lines(i,4)=lines(i,4)+(modifyx2-modifyx1)
			lines(i,5)=lines(i,5)+(modifyy2-modifyy1)
			lines(i,6)=lines(i,6)+(modifyz2-modifyz1)
		end if
	Next
	For i = 1 To circlec
		If circles(i,10)=2 Then circles(i,10)=1
		If circles(i,10)>0 Then
			If circles(i,12)>0 Then
				movingblock=TRUE
				movingblocks(circles(i,12))=TRUE
			EndIf
			circles(i,1)=circles(i,1)+(modifyx2-modifyx1)
			circles(i,2)=circles(i,2)+(modifyy2-modifyy1)
		EndIf
	Next
	If movingblock=TRUE Then
		For i = 1 To blockc
			If movingblocks(i)=TRUE Then
				'did the user try to rip the block apart
				'by selecting a portion of the block to move?
				'if so then what?
				'explode the block
				'or
				'create a new block
				'or
				'redifine the block and if there are other copies
				'of that block, update them to reflect the change
				'to do this right, the user needs to be aware
				'and prompted with a choice
				'1. proceed with modifying the block (if it's the only one)
				'2. proceed with modifying the block and it's coppies
				'3. explode the block
				'4. create a new block and give it a name
				'
				'for now, i'm gona just explode the block
				'and adjust blockc accordingly
				blockoffsets(i,1)=blockoffsets(i,1)+(modifyx2-modifyx1)
				blockoffsets(i,2)=blockoffsets(i,2)+(modifyy2-modifyy1)
				blockoffsets(i,3)=0
			EndIf
		Next
	EndIf
	degroup
	'redraw
	'escapeme
	initlinedraw
end Sub
Sub movedxfblock(mdbi As Integer)
	Dim As Integer i
	for i = 1 to linec
'		if lines(i,9)=mdbi And lines(i,8)=1 then
		if lines(i,9)=mdbi then
			lines(i,1)=lines(i,1)+(modifyx2-modifyx1)
			lines(i,2)=lines(i,2)+(modifyy2-modifyy1)
			lines(i,3)=lines(i,3)+(modifyz2-modifyz1)
			lines(i,4)=lines(i,4)+(modifyx2-modifyx1)
			lines(i,5)=lines(i,5)+(modifyy2-modifyy1)
			lines(i,6)=lines(i,6)+(modifyz2-modifyz1)
		end if
	Next
	For i = 1 To circlec
'		If circles(i,12)=mdbi And circles(i,10)=1 Then
		If circles(i,12)=mdbi Then
			circles(i,1)=circles(i,1)+(modifyx2-modifyx1)
			circles(i,2)=circles(i,2)+(modifyy2-modifyy1)
		EndIf
	Next
End Sub
sub setrotatecopydown()
	duplicate("setrotatecopydown")
end sub
sub setcopydown()
	duplicate("setcopydown")
end sub
sub degroup()
	Dim As Integer i
	groupexists=FALSE
	For i = 1 to linec
		if lines(i,8)=1 then lines(i,8)=0
		'If lines(i,9)>0 Then lines(i,8)=lines(i,9)
	Next
	For i = 1 To circlec
		if circles(i,10)=1 Then circles(i,10)=0
		'if circles(i,12)>0 Then circles(i,10)=circles(i,12)
	next
end Sub
Sub rotatepoint(rptx As Double,rpty As Double,rpivotx As Double,rpivoty As Double,rangle As Double)
	x1p=rptx-rpivotx
	y1p=rpty-rpivoty
	rotatedptx=x1p*cos(rangle*d2r) - y1p*sin(rangle*d2r)+rpivotx
	rotatedpty=y1p*cos(rangle*d2r) + x1p*sin(rangle*d2r)+rpivoty
End Sub
sub rotategroup()
	Dim As Integer i
	x1=modifyx1
	y1=modifyy1
	x2=modifyx2
	y2=modifyy2
	line (x1,y1)-(x2,y2)
	trackangle
	angle=angle-rotao
	circle (x1,y1),2
	for i = 1 to linec
		if lines(i,8)=1 Then
			x1p=lines(i,1)-modifyx1
			y1p=lines(i,2)-modifyy1
			x2p=lines(i,4)-modifyx1
			y2p=lines(i,5)-modifyy1
			x1=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
			y1=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
			x2=x2p*cos(angle*d2r) - y2p*sin(angle*d2r)+modifyx1
			y2=y2p*cos(angle*d2r) + x2p*sin(angle*d2r)+modifyy1
			line (x1,y1)-(x2,y2),lines(i,7)
		end If
	next
	For i = 1 To circlec
		If circles(i,10)=1 Then
			x1p=circles(i,1)-modifyx1
			y1p=circles(i,2)-modifyy1
			x1=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
			y1=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
			Select Case circles(i,9)
				Case 1
					Circle(x1,y1),circles(i,4),circles(i,5)
				Case 2
					selcircle=i
					calcarcendpoints
					x1p=arcendpoint1x-modifyx1
					y1p=arcendpoint1y-modifyy1
					x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					selarcangle
					arcstart=arcangle*d2r
					x1p=arcendpoint2x-modifyx1
					y1p=arcendpoint2y-modifyy1
					x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					selarcangle
					arcend=arcangle*d2r
					Circle(x1,y1),circles(i,4),circles(i,5),arcstart,arcend
				Case 3,4'rotating ellipses
					plotellipse(x1,y1,circles(i,3),circles(i,4),circles(i,5),circles(i,6),circles(i,7),circles(i,8),mymod(circles(i,11)+angle,360))
			End Select
		EndIf
	Next
end sub
sub setrotatedown()
	Dim As Integer i,j,k,c,m,n,p
	Dim As BOOLEAN rename_block,rename_blocks,dupe_blockname
	Dim blockstorotate(blockc) As BOOLEAN
	Dim As Integer selected_lines(),selected_lines_c,selected_circles(),selected_circles_c
	Dim As Integer linesinblock(),linesinblockc,circlesinblock(),circlesinblockc
	Dim As Integer templinescount,tempcirclescount
	selected_lines_c=0
	selected_circles_c=0
	for i = 1 to linec
		if lines(i,8)=1 Then
			selected_lines_c+=1
			If lines(i,9)>0 Then blockstorotate(lines(i,9))=TRUE
			x1p=lines(i,1)-modifyx1
			y1p=lines(i,2)-modifyy1
			x2p=lines(i,4)-modifyx1
			y2p=lines(i,5)-modifyy1
			lines(i,1)=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
			lines(i,2)=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
			lines(i,4)=x2p*cos(angle*d2r) - y2p*sin(angle*d2r)+modifyx1
			lines(i,5)=y2p*cos(angle*d2r) + x2p*sin(angle*d2r)+modifyy1
		end if
	Next
	For i = 1 To circlec
		If circles(i,10)=1 Then
			selected_circles_c+=1
			If circles(i,12)>0 Then blockstorotate(circles(i,12))=TRUE
			x1p=circles(i,1)-modifyx1
			y1p=circles(i,2)-modifyy1
			x1=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
			y1=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
			'circles(i,1)=x1
			'circles(i,2)=y1
			Select Case circles(i,9)
				Case 1
					circles(i,1)=x1
					circles(i,2)=y1
				Case 2
					selcircle=i
					calcarcendpoints
					x1p=arcendpoint1x-modifyx1
					y1p=arcendpoint1y-modifyy1
					x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					selarcangle
					arcstart=arcangle*d2r
					x1p=arcendpoint2x-modifyx1
					y1p=arcendpoint2y-modifyy1
					x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					selarcangle
					arcend=arcangle*d2r
					circles(i,1)=x1
					circles(i,2)=y1
					circles(i,6)=arcstart
					circles(i,7)=arcend
				Case 3,4'rotating ellipses
					circles(i,1)=x1
					circles(i,2)=y1
					circles(i,11)=mymod(circles(i,11)+angle,360)
			End Select
		EndIf
	Next
	initlinedraw
	For i = 1 To blockc
		If blockstorotate(i)=TRUE Then
			Select Case blockoffsets(i,4)
				Case 0,1
					Select Case blockoffsets(i,5)
						Case 0,1
							rename_block=FALSE
						Case Else
							rename_block=TRUE
					End Select
				Case Else
					rename_block=TRUE
			End Select
			If rename_block=FALSE Then
				x1p=blockoffsets(i,1)-modifyx1
				y1p=blockoffsets(i,2)-modifyy1
				blockoffsets(i,1)=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
				blockoffsets(i,2)=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
				blockoffsets(i,7)=mymod((blockoffsets(i,7)+angle),360)
			Else
				rename_blocks=TRUE
			EndIf
		EndIf
	Next
	If rename_blocks=TRUE Then
		templinescount=linec
		tempcirclescount=circlec
		ReDim selected_lines(selected_lines_c)
		ReDim selected_circles(selected_circles_c)
		c=0
		for i = 1 to linec
			if lines(i,8)=1 Then
				c+=1
				selected_lines(c)=i
			EndIf
		Next
		c=0
		For i = 1 To circlec	
			If circles(i,10)=1 Then
				c+=1
				selected_circles(c)=i
			EndIf
		Next
		degroup
		j=blockc
		For i = 1 To j
			If blockstorotate(i)=TRUE Then
				Select Case blockoffsets(i,4)
					Case 0,1
						Select Case blockoffsets(i,5)
							Case 0,1
								rename_block=FALSE
							Case Else
								rename_block=TRUE
						End Select
					Case Else
						rename_block=TRUE
				End Select
				If rename_block=TRUE Then
					'rename block
					theboxbelow("block "+ blocknames(i) +" is being renamed to "+ blocknames(i) +"-transformation.1 due to muilti transformation")
					'create new block with blockname + transfomation number
					c=0
					for k = 1 to linec
						if lines(k,9)=i Then
							c+=1
							lines(k,8)=1
						EndIf
					Next
					linesinblockc=c
					ReDim linesinblock(linesinblockc)
					c=0
					for k = 1 to linec
						if lines(k,9)=i Then
							c+=1
							linesinblock(c)=k
						EndIf
					Next
					c=0
					For k = 1 To circlec	
						If circles(k,12)=i Then
							c+=1
							circles(k,10)=1
						EndIf
					Next
					circlesinblockc=c
					ReDim circlesinblock(circlesinblockc)
					c=0
					For k = 1 To circlec	
						If circles(k,12)=i Then
							c+=1
							circlesinblock(c)=k
						EndIf
					Next
					explode_block
					For k=1 To linesinblockc
						lines(linesinblock(k),8)=1
					Next
					For k=1 To circlesinblockc
						circles(circlesinblock(k),10)=1
					Next
					groupexists=TRUE
					If InStr(blocknames(i),"_trans_ver.")<>0 Then
						c=InStr(blocknames(i),"_trans_ver.")
						m=0
						Do
							m+=1
							blockname=Left(blocknames(i),c-1)+"_trans_ver."+LTrim(Str(ValInt(Mid(blocknames(i),c+11))+m))
							dupe_blockname=FALSE
							For n=1 To blockc	
								If blockname=blocknames(n) Then
									dupe_blockname=TRUE
									Exit For
								EndIf
							Next
							If dupe_blockname=FALSE Then Exit Do
						Loop
					Else
						blockname=blocknames(i)+"_trans_ver.0"
						c=InStr(blockname,"_trans_ver.")
						m=0
						Do
							m+=1
							blockname=Left(blockname,c-1)+"_trans_ver."+LTrim(Str(m))
							dupe_blockname=FALSE
							For n=1 To blockc	
								If blockname=blocknames(n) Then
									dupe_blockname=TRUE
									Exit For
								EndIf
							Next
							If dupe_blockname=FALSE Then Exit Do
						Loop
					EndIf
					
					selbutton=111'create block
					buttonmanager
					degroup
				EndIf
			EndIf
		Next
		groupexists=TRUE
		For i=1 To selected_lines_c
			If lines(selected_lines(i),8)<>-1 Then lines(selected_lines(i),8)=1
		Next 
		For i=templinescount+1 To linec
			If lines(i,8)<>-1 Then lines(i,8)=1
		Next
		For i=1 To selected_circles_c
			If circles(selected_circles(i),10)<>-1 Then circles(selected_circles(i),10)=1
		Next 
		For i=tempcirclescount+1 To circlec
			If circles(i,10)<>-1 Then circles(i,10)=1
		Next
		redraw
	EndIf
End sub
sub rotateblock(bntr As Integer)
	Dim As Integer i
	for i = 1 to linec
		if lines(i,9)=bntr Then
			x1p=lines(i,1)-modifyx1
			y1p=lines(i,2)-modifyy1
			x2p=lines(i,4)-modifyx1
			y2p=lines(i,5)-modifyy1
			lines(i,1)=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
			lines(i,2)=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
			lines(i,4)=x2p*cos(angle*d2r) - y2p*sin(angle*d2r)+modifyx1
			lines(i,5)=y2p*cos(angle*d2r) + x2p*sin(angle*d2r)+modifyy1
		end if
	Next
	For i = 1 To circlec
		If circles(i,12)=bntr Then
			x1p=circles(i,1)-modifyx1
			y1p=circles(i,2)-modifyy1
			x1=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
			y1=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
			Select Case circles(i,9)
				Case 1
					circles(i,1)=x1
					circles(i,2)=y1
				Case 2
					selcircle=i
					calcarcendpoints
					x1p=arcendpoint1x-modifyx1
					y1p=arcendpoint1y-modifyy1
					x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					selarcangle
					arcstart=arcangle*d2r
					x1p=arcendpoint2x-modifyx1
					y1p=arcendpoint2y-modifyy1
					x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					selarcangle
					arcend=arcangle*d2r
					circles(i,1)=x1
					circles(i,2)=y1
					circles(i,6)=arcstart
					circles(i,7)=arcend
				Case 3,4'rotating ellipses
					circles(i,1)=x1
					circles(i,2)=y1
					circles(i,11)=mymod(circles(i,11)+angle,360)
			End Select
		EndIf
	Next
'	initlinedraw
end sub
sub selarcangle()
		xlength=x2-x1
		ylength=y2-y1
		arcangle=atan2(ylength,xlength)*r2d
		arcangle+=360
		arcangle=mymod(arcangle,360)
End Sub
sub flipvertical()
	Dim As Integer i
	Dim As BOOLEAN flippingblock,flippingblocks(blockc)
	Dim  As Double arclenindegrees,arclengthinradians
	for i = 1 to linec
		if lines(i,8)=1 then
			If lines(i,9)>0 Then
				flippingblock=TRUE
				flippingblocks(lines(i,9))=TRUE
			EndIf
			lines(i,2)=modifyy1+(modifyy1-lines(i,2))
			lines(i,5)=modifyy1+(modifyy1-lines(i,5))
		end if
	Next
	for i = 1 to circlec
		if circles(i,10)=1 Then
			If circles(i,12)>0 Then
				flippingblock=TRUE
				flippingblocks(circles(i,12))=TRUE
			EndIf
			Select Case circles(i,9)
				Case 1
					circles(i,2)=modifyy1+(modifyy1-circles(i,2))
				Case 2
					If circles(i,6)>circles(i,7) Then
						arclengthinradians= (PI*2)-circles(i,6)+circles(i,7)
					Else
						arclengthinradians=circles(i,7)-circles(i,6)
					EndIf
					selcircle=i
					calcarcendpoints
					x1=circles(i,1)
					y1=modifyy1+(modifyy1-circles(i,2))
					y1p=modifyy1+(modifyy1-arcendpoint2y)'use this for vertical
					x2=arcendpoint2x
					y2=y1p
					selarcangle
					arcstart=arcangle*d2r
					arcend=arcstart+arclengthinradians
					circles(i,2)=modifyy1+(modifyy1-circles(i,2))
					circles(i,6)=arcstart
					circles(i,7)=arcend
				Case 3,4
					circles(i,2)=modifyy1+(modifyy1-circles(i,2))
					tempdouble=360-circles(i,6)-circles(i,7)
					circles(i,6)=circles(i,6)+tempdouble
					circles(i,7)=circles(i,7)+tempdouble
					circles(i,6)=mymod(circles(i,6),360)
					circles(i,7)=mymod(circles(i,7),360)
					circles(i,11)=360-circles(i,11)
					circles(i,11)=mymod(circles(i,11),360)
					If circles(i,6)=circles(i,7) Then
						circles(i,6)=0
						circles(i,7)=360
					EndIf
			End Select
		end if
	Next
	If flippingblock=TRUE Then
		For i = 1 To blockc
			If flippingblocks(i)=TRUE Then
				blockoffsets(i,2)=modifyy1+(modifyy1-blockoffsets(i,2))
				blockoffsets(i,5)=blockoffsets(i,5)*-1
			EndIf
		Next
	EndIf
End sub
sub fliphorizontal()
	Dim As Integer i
	Dim As BOOLEAN flippingblock,flippingblocks(blockc)
	Dim As Double arclenindegrees,arclengthinradians
	for i = 1 to linec
		if lines(i,8)=1 then
			If lines(i,9)>0 Then
				flippingblock=TRUE
				flippingblocks(lines(i,9))=TRUE
			EndIf
			lines(i,1)=modifyx1+(modifyx1-lines(i,1))
			lines(i,4)=modifyx1+(modifyx1-lines(i,4))
		end if
	Next
	for i = 1 to circlec
		if circles(i,10)=1 Then
			If circles(i,12)>0 Then
				flippingblock=TRUE
				flippingblocks(circles(i,12))=TRUE
			EndIf
			Select Case circles(i,9)
				Case 1
					circles(i,1)=modifyx1+(modifyx1-circles(i,1))
				Case 2
					If circles(i,6)>circles(i,7) Then
						arclengthinradians= (PI*2)-circles(i,6)+circles(i,7)
					Else
						arclengthinradians=circles(i,7)-circles(i,6)
					EndIf
					selcircle=i
					calcarcendpoints
					x1=modifyx1+(modifyx1-circles(i,1))
					y1=circles(i,2)
					x1p=modifyx1+(modifyx1-arcendpoint2x)
					x2=x1p
					y2=arcendpoint2y
					selarcangle
					arcstart=arcangle*d2r
					arcend=arcstart+arclengthinradians
					circles(i,1)=modifyx1+(modifyx1-circles(i,1))
					circles(i,6)=arcstart
					circles(i,7)=arcend
				Case 3,4
					circles(i,1)=modifyx1+(modifyx1-circles(i,1))
					tempdouble=360-circles(i,7)-circles(i,6)
					circles(i,6)=circles(i,6)+tempdouble+180
					circles(i,7)=circles(i,7)+tempdouble+180
					circles(i,6)=mymod(circles(i,6),360)
					circles(i,7)=mymod(circles(i,7),360)
					circles(i,11)=360-circles(i,11)
					circles(i,11)=mymod(circles(i,11),360)
					If circles(i,6)=circles(i,7) Then
						circles(i,6)=0
						circles(i,7)=360
					EndIf
			End Select
		end if
	Next
	If flippingblock=TRUE Then
		For i = 1 To blockc
			If flippingblocks(i)=TRUE Then
				blockoffsets(i,1)=modifyx1+(modifyx1-blockoffsets(i,1))
				blockoffsets(i,4)=blockoffsets(i,4)*-1
			EndIf
		Next
	EndIf

end sub
sub flipverticalcopy()
	duplicate("flipverticalcopy")
end sub
sub fliphorizontalcopy()
	duplicate("fliphorizontalcopy")
end Sub
sub flipvh()
	flipvertical
	fliphorizontal
End sub
sub flipvhcopy()
	flipvertical
	fliphorizontalcopy
	flipvertical
end sub
Sub plotellipse(ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double)
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	Dim As Double i
	Dim As Double eplotx,eploty,eplotxp,eplotyp,easpect,eresolution,elength
	Dim As BOOLEAN eplot
	Dim As Double f1x1,f1y1,f2x1,f2y1
	Dim As Double enormal
	
	If estart>eend Then
		elength=360+eend
	Else
		elength=eend
	EndIf
	
	
	If er1>er2 Then
		foci=Sqr(er1^2-er2^2)
		f1x1=ex1-cos(eangle*d2r)*foci
		f1y1=ey1-sin(eangle*d2r)*foci
		f2x1=ex1+cos(eangle*d2r)*foci
		f2y1=ey1+sin(eangle*d2r)*foci
	Else
		foci=Sqr(er2^2-er1^2)
		f1x1=ex1+cos((eangle+90)*d2r)*foci
		f1y1=ey1+sin((eangle+90)*d2r)*foci
		f2x1=ex1-cos((eangle+90)*d2r)*foci
		f2y1=ey1-sin((eangle+90)*d2r)*foci
	End If
		
	If showellipsefoci=TRUE then
		Circle(f1x1,f1y1),2,10
		Circle(f2x1,f2y1),2,10
	End If
	
	eplot=FALSE
	If detectingpoints=1 Then eresolution=1 Else eresolution=1

	'plotellipse
	For i = estart To elength Step eresolution
		'Sleep 1
		'x1p=Cos(i*d2r)*er1
		'y1p=Sin(i*d2r)*er2
		eplotx=Cos(i*d2r)*er1*Cos(eangle*d2r) - Sin(i*d2r)*er2*Sin(eangle*d2r)+ex1
		eploty=Cos(i*d2r)*er1*Sin(eangle*d2r) + Sin(i*d2r)*er2*Cos(eangle*d2r)+ey1

		If eplot=false Then
			eplot=TRUE
		Else
			If plotellipseoffset=TRUE Then
				enormal=calcellipsenorm2(eplotx,eploty,f1x1,f1y1,f2x1,f2y1)
				eplotx+=Cos(enormal*para_direction*d2r)*para_offset
				eploty+-Sin(enormal*para_direction*d2r)*para_offset
			EndIf
		EndIf
		Line(eplotx,eploty)-(eplotxp,eplotyp),ecolor
		eplotxp=eplotx
		eplotyp=eploty
	Next
	x1p=Cos(eend*d2r)*er1
	y1p=Sin(eend*d2r)*er2
	eplotx=x1p*Cos(eangle*d2r) - y1p*Sin(eangle*d2r)+ex1
	eploty=y1p*Cos(eangle*d2r) + x1p*Sin(eangle*d2r)+ey1
	If plotellipseoffset=TRUE Then
		plotellipseoffset=FALSE
		enormal=calcellipsenorm2(eplotx,eploty,f1x1,f1y1,f2x1,f2y1)
		eplotx+=Cos(enormal*para_direction*d2r)*para_offset
		eploty+-Sin(enormal*para_direction*d2r)*para_offset
	EndIf
	Line(eplotx,eploty)-(eplotxp,eplotyp),ecolor
End Sub

Function calcellipsenorm2(poex As Double,poey As Double,foci1x As Double,foci1y As Double,foci2x As Double,foci2y As Double) As Double
	Dim As Double ptf1a,ptf2a,ptfba
	ptf1a=abtp(poex,poey,0,foci1x,foci1y,0)
	ptf2a=abtp(poex,poey,0,foci2x,foci2y,0)
	Select Case ptf1a
		Case ptf2a
			ptfba=ptf1a
		'ok since the angle between the foci can not be > 180
		'i can just split it in order to calc bisector angle
		Case Is < ptf2a
			If ptf2a-ptf1a<180 Then
				ptfba=ptf1a+(ptf2a-ptf1a)/2
			Else
				ptfba=ptf2a+(ptf1a-ptf2a+360)/2
			EndIf
		Case Else
			If ptf1a-ptf2a<180 Then
				ptfba=ptf2a+(ptf1a-ptf2a)/2
			Else
				ptfba=ptf1a+(ptf2a-ptf1a+360)/2
			EndIf
	End Select
	ptfba=ptfba+180
	Return ptfba
End Function

Sub plotellipse_gcode(ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double)
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group

	Dim As Double i
	Dim As Double eplotx,eploty,eplotxp,eplotyp,easpect,eresolution,elength
	Dim As BOOLEAN eplot
	Dim As Double foci1x,foci1y,foci2x,foci2y
	Dim As Double ptf1a,ptf2a,ptfba
	Dim As Double gplotva,gplotvap
	Dim As BOOLEAN gplotvac
	Dim cusp As Double
	gplotvac=FALSE
	
	'depending on my resolution setting
	'default is 360 degrees on 1 degree increments
	'i need an array to store the values of x&y for each point plotted
	'and in case i need to plot it backwards then just reverse the output
	'
	'i made these global instead
	'Dim As Double gplotx(360),gploty(360),gplotz(360)
	'Dim As Integer gplotc
	gplotc=0
	'I attribute cusp to:
	'http://mathdl.maa.org/mathDL/46/?pa=content&sa=viewDocument&nodeId=2554&pf=1
	'by Frederick Hartmann and Robert Jantzen
	'If er1<er2 then
	'	'easpect=er1/er2
	'	'eresolution=15-er2/(drawareax2-drawareax1)*20
	'	cusp=Sqr(er2^2-er1^2)^2/er2
	'	If gplotoffsetr>er2-cusp Then gplotoffsetr=er2-cusp
	'Else
	'	'easpect=er2/er1
	'	'eresolution=15-er1/(drawareax2-drawareax1)*20
	'	cusp=Sqr(er1^2-er2^2)^2/er1
	'	If gplotoffsetr>er1-cusp Then gplotoffsetr=er1-cusp
	'End If
	
	'easpect=er1/er2
	
	'eresolution=15-er2/(drawareax2-drawareax1)*20
	'the idea of using eres was to speed up things but it was messing up
	'so i just chose to draw every 6 degrees instead
	
	If estart>eend Then
		'elength=estart+360-estart+eend
		'what estart-estart=zero
		'should be 360+eend
		elength=360+eend
	Else
		elength=eend
	EndIf
	eplot=FALSE
	If detectingpoints=1 Then eresolution=1 Else eresolution=1
	If gplotoffset=TRUE Then
		If er1>er2 Then
			foci=Sqr(er1^2-er2^2)
			foci1x=ex1+cos(eangle*d2r)*foci
			foci1y=ey1+sin(eangle*d2r)*foci
			foci2x=ex1-cos(eangle*d2r)*foci
			foci2y=ey1-sin(eangle*d2r)*foci
			'Circle(foci1x,foci1y),2,10
			'Circle(foci2x,foci2y),2,10
		Else
			foci=Sqr(er2^2-er1^2)
			foci1x=ex1+cos((eangle+90)*d2r)*foci
			foci1y=ey1+sin((eangle+90)*d2r)*foci
			foci2x=ex1-cos((eangle+90)*d2r)*foci
			foci2y=ey1-sin((eangle+90)*d2r)*foci
			'Circle(foci1x,foci1y),2,10
			'Circle(foci2x,foci2y),2,10
		End If
	End If

	'plotellipse
	For i = estart To elength Step eresolution
		'Sleep 1
		'x1p=Cos(i*d2r)*er1
		'y1p=Sin(i*d2r)*er2
		eplotx=Cos(i*d2r)*er1*Cos(eangle*d2r) - Sin(i*d2r)*er2*Sin(eangle*d2r)+ex1
		eploty=Cos(i*d2r)*er1*Sin(eangle*d2r) + Sin(i*d2r)*er2*Cos(eangle*d2r)+ey1
	'Dim As Double foci1x,foci1y,foci2x,foci2y
	'Dim As Double ptf1a,ptf2a,ptfba
		If gplotoffset=TRUE Then
			gplotc=gplotc+1
			ptf1a=abtp(eplotx,eploty,0,foci1x,foci1y,0)
			ptf2a=abtp(eplotx,eploty,0,foci2x,foci2y,0)
			Select Case ptf1a
				Case ptf2a
					ptfba=ptf1a
				'ok since the angle between the foci can not be > 180
				'i can just split it in order to calc bisector angle
				Case Is < ptf2a
					If ptf2a-ptf1a<180 Then
						ptfba=ptf1a+(ptf2a-ptf1a)/2
					Else
						ptfba=ptf2a+(ptf1a-ptf2a+360)/2
					EndIf
				Case Else
					If ptf1a-ptf2a<180 Then
						ptfba=ptf2a+(ptf1a-ptf2a)/2
					Else
						ptfba=ptf1a+(ptf2a-ptf1a+360)/2
					EndIf
			End Select
			'ok now am i doing an inward or outward cut?
			'lets assume its an outward cut for now
			'gplotoffsetr=10 bit r=radius
			'gplotoffsetd="outward" d=direction
			'Select Case gplotoffsetd
			'	Case "inward"
			'		ptfba=ptfba+0
			'		gplotx(gplotc)=eplotx+Cos(ptfba*d2r)*gplotoffsetr
			'		gploty(gplotc)=eploty+Sin(ptfba*d2r)*gplotoffsetr
			'		PSet(gplotx(gplotc),gploty(gplotc)),10
			'	Case "outward"
			'		ptfba=ptfba+180
			'		gplotx(gplotc)=eplotx+Cos(ptfba*d2r)*gplotoffsetr
			'		gploty(gplotc)=eploty+Sin(ptfba*d2r)*gplotoffsetr
			'		PSet(gplotx(gplotc),gploty(gplotc)),10
			'End Select
			'atolength()
			'fx=x1+cos(angle*d2r)*newlength
			'fy=y1+sin(angle*d2r)*newlength
			'gplotx(gplotc)=eplotx+Cos(ptfba*d2r)*gplotoffsetr
			'gploty(gplotc)=eploty+Sin(ptfba*d2r)*gplotoffsetr
			'PSet(gplotx(gplotc),gploty(gplotc)),10

			ptfba=ptfba+0
			gplotx(gplotc)=eplotx+Cos(ptfba*d2r)*gplotoffsetr
			gploty(gplotc)=eploty+Sin(ptfba*d2r)*gplotoffsetr
			PSet(gplotx(gplotc),gploty(gplotc))
			'Line(eplotx,eploty)-(gplotx(gplotc),gploty(gplotc))
			'Circle(gplotx(gplotc),gploty(gplotc)),gplotoffsetr
			'If gplotvac=TRUE Then gplotvac=FALSE
			'''If gplotc>2 Then
			'''	gplotva=abtp(gplotx(gplotc-1),gploty(gplotc-1),0,gplotx(gplotc),gploty(gplotc),0)
			'''	If Abs(gplotva-gplotvap) > 90 Then
			'''		gplotvac=TRUE
			'''		'sleep
			'''	EndIf
			'''EndIf
			'''If gplotvac=TRUE Then
			'''	Line(eplotx,eploty)-(gplotx(gplotc),gploty(gplotc))
			'''	Circle(gplotx(gplotc),gploty(gplotc)),gplotoffsetr
			'''	gplotvac=FALSE
			'''EndIf
			ptfba=ptfba+180
			gplotx(gplotc)=eplotx+Cos(ptfba*d2r)*gplotoffsetr
			gploty(gplotc)=eploty+Sin(ptfba*d2r)*gplotoffsetr
			PSet(gplotx(gplotc),gploty(gplotc)),10
			'Line(eplotx,eploty)-(gplotx(gplotc),gploty(gplotc)),10
			'Circle(gplotx(gplotc),gploty(gplotc)),gplotoffsetr,10
			Sleep 10
		Else
			gplotx(gplotc)=eplotx
			gploty(gplotc)=eploty
		EndIf
		If gplotc>1 Then gplotvap=abtp(gplotx(gplotc-1),gploty(gplotc-1),0,gplotx(gplotc),gploty(gplotc),0)

		'If eplot=false Then
		'	eplot=TRUE
		'Else
		'	'instead of drawing a line here - just print the values
		'	Line(eplotx,eploty)-(eplotxp,eplotyp),ecolor
		'EndIf
		'eplotxp=eplotx
		'eplotyp=eploty
	Next
	x1p=Cos(eend*d2r)*er1
	y1p=Sin(eend*d2r)*er2
	eplotx=x1p*Cos(eangle*d2r) - y1p*Sin(eangle*d2r)+ex1
	eploty=y1p*Cos(eangle*d2r) + x1p*Sin(eangle*d2r)+ey1
	'If eplotx<>gplotx(gplotc) Or eploty<>gploty(gplotc) Then
	'	gplotc=gplotc+1
	'	If gplotoffset=TRUE Then
	'		
	'	Else
	'		gplotx(gplotc)=eplotx
	'		gploty(gplotc)=eploty
	'	EndIf
	'EndIf 
	''instead of drawing a line here - just print the values
	'Line(eplotx,eploty)-(eplotxp,eplotyp),ecolor
	'
	'for tool offset - or tool width offset
	'a parallel to a an ellipse is a plot of the end points of lines
	'that are perpendicular to the ellipse. the angle of each line
	'is the point on ellipse to foci bisector angle.
	'depending on inward or outward parallel projection,
	'the angle will be 180 degrees out for outward projection
	'and 0 degrees out for inward projection.
	'length of the perpendicular line then determines parallel offset
	'distance of plotted points.
	'in the application of milling: the offset distance is the radius
	'of a drill bit which can then be centered on these plotted points
	'in order to cut around the elliptical curve.
	'in the application of laser cutting or when the radius of bit
	'is extreemly small, then perhaps there would not be any need to
	'calculate this offset.
	'If showellipsefoci=TRUE then
	'	Dim As Double tx1,ty1
	'	If er1>er2 Then
	'		foci=Sqr(er1^2-er2^2)
	'		tx1=ex1+cos(eangle*d2r)*foci
	'		ty1=ey1+sin(eangle*d2r)*foci
	'		Circle(tx1,ty1),2,10
	'		tx1=ex1-cos(eangle*d2r)*foci
	'		ty1=ey1-sin(eangle*d2r)*foci
	'		Circle(tx1,ty1),2,10
	'	Else
	'		foci=Sqr(er2^2-er1^2)
	'		tx1=ex1+cos((eangle+90)*d2r)*foci
	'		ty1=ey1+sin((eangle+90)*d2r)*foci
	'		Circle(tx1,ty1),2,10
	'		tx1=ex1-cos((eangle+90)*d2r)*foci
	'		ty1=ey1-sin((eangle+90)*d2r)*foci
	'		Circle(tx1,ty1),2,10
	'	End if
	'End If

End Sub
Function calcellipsenorm(ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double) As Double
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group

	'Dim As Double eplotx,eploty,eplotxp,eplotyp,easpect,eresolution,elength
	'Dim As Byte eplot
	Dim As Double foci1x,foci1y,foci2x,foci2y
	Dim As Double ptf1a,ptf2a,ptfba
	If er1>er2 Then
		foci=Sqr(er1^2-er2^2)
		foci1x=ex1+cos(eangle*d2r)*foci
		foci1y=ey1+sin(eangle*d2r)*foci
		foci2x=ex1-cos(eangle*d2r)*foci
		foci2y=ey1-sin(eangle*d2r)*foci
		'Circle(foci1x,foci1y),2,10
		'Circle(foci2x,foci2y),2,10
	Else
		foci=Sqr(er2^2-er1^2)
		foci1x=ex1+cos((eangle+90)*d2r)*foci
		foci1y=ey1+sin((eangle+90)*d2r)*foci
		foci2x=ex1-cos((eangle+90)*d2r)*foci
		foci2y=ey1-sin((eangle+90)*d2r)*foci
		'Circle(foci1x,foci1y),2,10
		'Circle(foci2x,foci2y),2,10
	End If
	ptf1a=abtp(fxm,fym,0,foci1x,foci1y,0)
	ptf2a=abtp(fxm,fym,0,foci2x,foci2y,0)
	Select Case ptf1a
		Case ptf2a
			ptfba=ptf1a
		'ok since the angle between the foci can not be > 180
		'i can just split it in order to calc bisector angle
		Case Is < ptf2a
			If ptf2a-ptf1a<180 Then
				ptfba=ptf1a+(ptf2a-ptf1a)/2
			Else
				ptfba=ptf2a+(ptf1a-ptf2a+360)/2
			EndIf
		Case Else
			If ptf1a-ptf2a<180 Then
				ptfba=ptf2a+(ptf1a-ptf2a)/2
			Else
				ptfba=ptf1a+(ptf2a-ptf1a+360)/2
			EndIf
	End Select
	ptfba=ptfba+180
	ptfba=mymod(ptfba,360)
	Return ptfba
End Function
Sub boxarc(j As Integer,ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double)
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group

		'boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))


	Dim As Double i
	Dim As Double eplotx,eploty,eplotxp,eplotyp,easpect,eresolution,elength
	Dim As BOOLEAN eplot

	'easpect=er1/er2
	If estart>eend Then
		elength=estart+360-estart+eend
	Else
		elength=eend
	EndIf

	'eresolution=15-er2/(drawareax2-drawareax1)*20
	eplot=false
	If detectingpoints=1 Then eresolution=1 Else eresolution=1
	'boxarc
	For i = estart To elength Step eresolution
		eplotx=ex1+(Cos(i*d2r)*er1)
		eploty=ey1+(Sin(i*d2r)*er2)
		x1p=eplotx-ex1
		y1p=eploty-ey1
		eplotx=x1p*Cos(eangle*d2r) - y1p*Sin(eangle*d2r)+ex1
		eploty=y1p*Cos(eangle*d2r) + x1p*Sin(eangle*d2r)+ey1
		If eplot=false Then
			eplot=TRUE
			boxarcx1=eplotx
			boxarcy1=eploty
			boxarcx2=eplotx
			boxarcy2=eploty
'			boxarcx3=eplotx
'			boxarcy3=eploty
'			boxarcx4=eplotx
'			boxarcy4=eploty
		Else
			'Line(eplotx,eploty)-(eplotxp,eplotyp),10
			If eplotx<boxarcx1 Then boxarcx1=eplotx
			If eplotx>boxarcx2 Then boxarcx2=eplotx
			If eploty<boxarcy1 Then boxarcy1=eploty
			If eploty>boxarcy2 Then boxarcy2=eploty
		Endif
		eplotxp=eplotx
		eplotyp=eploty
	Next
	eplotx=ex1+(Cos(eend*d2r)*er1)
	eploty=ey1+(Sin(eend*d2r)*er2)
	x1p=eplotx-ex1
	y1p=eploty-ey1
	eplotx=x1p*Cos(eangle*d2r) - y1p*Sin(eangle*d2r)+ex1
	eploty=y1p*Cos(eangle*d2r) + x1p*Sin(eangle*d2r)+ey1
	If eplotx<boxarcx1 Then boxarcx1=eplotx
	If eplotx>boxarcx2 Then boxarcx2=eplotx
	If eploty<boxarcy1 Then boxarcy1=eploty
	If eploty>boxarcy2 Then boxarcy2=eploty
End Sub
Sub createellipse()
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group
	memmanagecircle
	circles(circlec,1)=x1
	circles(circlec,2)=y1
	circles(circlec,4)=radius
	circles(circlec,5)=lc

	If Abs(Abs(radius)-Abs(eradius))<eliptolerance Then
		'this is a circle or an arc
		if x1=x2 and y1=y2 Then'not sure why x&x and y&y would be equal
			circlec=circlec-1'but it's purpose was to not use this ellipse / circle
			'perhpas it was to do with creating splines
		else
			'convert ellipse to circle or arc
			If arcstart=0 And arcend=360 Then
				circles(circlec,9)=1
			Else
				arcstart=arcstart+erotation
				arcend=arcend+erotation
				circles(circlec,9)=2
				circles(circlec,6)=PI*arcstart/180
				circles(circlec,7)=PI*arcend/180
			EndIf
		end If
	Else
		'this is an ellipse
		'circles(circlec,1)=x1
		'circles(circlec,2)=y1
		'circles(circlec,4)=radius
		'circles(circlec,5)=lc
		circles(circlec,6)=mymod(arcstart,360)
		circles(circlec,7)=mymod(arcend,360)
		circles(circlec,8)=eradius
		If arcstart=0 And arcend=360 Then
			circles(circlec,9)=3
		Else
			circles(circlec,9)=4
		EndIf
		circles(circlec,11)=mymod(erotation,360)
	'	screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
		if x1=x2 and y1=y2 then
			circlec=circlec-1
		else
			plotellipse(x1,y1,z1,radius,lc,arcstart,arcend,eradius,erotation)
		end If
		If showellipsefoci=TRUE Then
			Dim As Double tx1,ty1
			'shift_key=1
			tx1=x1
			ty1=y1
			If radius>eradius Then
				foci=Sqr(radius^2-eradius^2)
				x1=tx1+cos(erotation*d2r)*foci
				y1=ty1+sin(erotation*d2r)*foci
				length=2
				createcircle
				x1=tx1-cos(erotation*d2r)*foci
				y1=ty1-sin(erotation*d2r)*foci
				createcircle
			Else
				foci=Sqr(eradius^2-radius^2)
				x1=tx1+cos((erotation+90)*d2r)*foci
				y1=ty1+sin((erotation+90)*d2r)*foci
				length=2
				createcircle
				x1=tx1-cos((erotation+90)*d2r)*foci
				y1=ty1-sin((erotation+90)*d2r)*foci
				createcircle
			End If
		End If
	EndIf
	If splining=FALSE Then
		x1=x2
		y1=y2
		drawing=false
		snap=false
		initlinedraw
	End If
End Sub
Sub calcperpintersects(px1 As Double,py1 As Double,pa1 As Double,px2 As Double,py2 As Double,pa2 As Double)
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det

	ax1=px1
	ay1=py1
	ax2=px1+cos((pa1+90)*d2r)*100
	ay2=py1+sin((pa1+90)*d2r)*100
	
	bx1=px2
	by1=py2
	bx2=px2+cos((pa2+90)*d2r)*100
	by2=py2+sin((pa2+90)*d2r)*100
	
	
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	
	det = A1*B2 - A2*B1
	If det = 0 Then
		intersection="parallel"
	Else
		pfxm = (B2*C1 - B1*C2)/det
		pfym = (A1*C2 - A2*C1)/det
	End if
End Sub
Sub calcellipseintersects(px1 As Double,py1 As Double,pa1 As Double,px2 As Double,py2 As Double,pa2 As Double)
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	ax1=px1
	ay1=py1
	ax2=px1+cos((pa1)*d2r)*100
	ay2=py1+sin((pa1)*d2r)*100
	
	bx1=px2
	by1=py2
	bx2=px2+cos((pa2)*d2r)*100
	by2=py2+sin((pa2)*d2r)*100
	
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	
	det = A1*B2 - A2*B1
	If det = 0 Then
		intersection="parallel"
		'just create arc between both lines based on angle of second line (maybe)
	Else
		pfxm = (B2*C1 - B1*C2)/det
		pfym = (A1*C2 - A2*C1)/det
	End If
	'Circle (pfxm,pfym),5,11
'	calcmidpoint()
'	fxm=(lines(selline,1)+lines(selline,4))/2
'	fym=(lines(selline,2)+lines(selline,5))/2
	'fxm=(px1+px2)/2
	'fym=(py1+py2)/2
	
'	calcselange
'	xlength=lines(selline,4)-lines(selline,1)
'	ylength=lines(selline,5)-lines(selline,2)
'	selangle=atan2(ylength,xlength)*r2d
'	if selangle<0 then selangle=360+selangle
	
	Dim As Double tangle,tangletest,pfxmtest,pfymtest
	pfxmtest=pfxm
	pfymtest=pfym
	xlength=(px1+px2)/2-pfxm
	ylength=(py1+py2)/2-pfym
	tangle=atan2(ylength,xlength)*r2d
	tangle+=360
	tangle=mymod(tangle,360)
	
	ax1=px1
	ay1=py1
	ax2=px1+cos((pa1+90)*d2r)*100
	ay2=py1+sin((pa1+90)*d2r)*100
'	
	bx1=pfxm
	by1=pfym
	bx2=pfxm+cos(tangle*d2r)*100
	by2=pfym+sin(tangle*d2r)*100
'Circle (bx2,by2),5,15

	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	
	det = A1*B2 - A2*B1
	If det = 0 Then
		intersection="parallel"
	Else
		pfxm = (B2*C1 - B1*C2)/det
		pfym = (A1*C2 - A2*C1)/det
	End If
	'this is the exception
	If det=0 Or py1 > py2 Then
		'create an Arc (Not an Ellipse)
		'calc mid point of line between second line and intersection of it's angle with first line
		'calc center of arc by calcing intersection of perp from that midpoint
		'show arc and line segment (line segment from 2nd line to arc)
		intersection="outside"
	EndIf

	'Line(pfxm,pfym)-((px1+px2)/2,(py1+py2)/2),10
	'calc then angle of this line
	'if it is not equal to tangle then
	'do this all again BUT swap
	xlength=pfxm-(px1+px2)/2
	ylength=pfym-(py1+py2)/2
	tangletest=atan2(ylength,xlength)*r2d
	if tangletest<0 then tangletest=360+tangletest
	'Exit Sub
	Select Case tangletest
		Case tangle-1 To tangle +1
			'should be correct pfxm,pfym
			reversed=FALSE
		Case Else
			reversed=TRUE
	End Select
End Sub
Sub calclinelineintersection()
	'thanx to topcoder.com/tc?module=Static&d1=tutorials&d2=geometry2
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	
	intersection=""
	
	intersectioninsidefirstline=FALSE
	intersectioninsidesecondline=FALSE
	intersectioninsidebothlines=FALSE
	
	ax1=lines(aintersect,1)
	ay1=lines(aintersect,2)
	ax2=lines(aintersect,4)
	ay2=lines(aintersect,5)
	bx1=lines(bintersect,1)
	by1=lines(bintersect,2)
	bx2=lines(bintersect,4)
	by2=lines(bintersect,5)
	
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	
	det = A1*B2 - A2*B1
	If det = 0 Then
		intersection="parallel"
	Else
		'intersection = "inside"
		fxm = (B2*C1 - B1*C2)/det
		'fxm=( (bx1-bx2) * ((ay2-ay1)*ax1+(ax1-ax2)*ay1) - (ax1-ax2) * ((by2-by1)*bx1+(bx1-bx2)*by1) ) / (det)
		fym = (A1*C2 - A2*C1)/det
		
		Select Case fxm
			Case ax1 To ax2,ax2 To ax1
				Select Case fym
					Case ay1 To ay2,ay2 To ay1
						intersectioninsidefirstline=TRUE
				End Select
		End Select
		Select Case fxm
			Case bx1 To bx2,bx2 To bx1
				Select Case fym
					Case by1 To by2,by2 To by1
						intersectioninsidesecondline=TRUE
				End Select
		End Select
		If intersectioninsidefirstline=TRUE And intersectioninsidesecondline=TRUE Then
			intersectioninsidebothlines=TRUE
			intersection="inside"
		Else
			intersection="outside"
		EndIf
	End If
End Sub
Sub calclinelineintersection2(ax1 As Double,ay1 As Double,ax2 As Double,ay2 As Double,bx1 As Double,by1 As Double,bx2 As Double,by2 As Double)
	'thanx to topcoder.com/tc?module=Static&d1=tutorials&d2=geometry2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	fxm = (B2*C1 - B1*C2)/det
	fym = (A1*C2 - A2*C1)/det
End Sub
Sub calclli(ax1 As Double,ay1 As Double,ax2 As Double, ay2 As Double,bx1 As Double,by1 As Double,bx2 As Double, by2 As Double)
	'thanx to topcoder.com/tc?module=Static&d1=tutorials&d2=geometry2
	'Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	
	intersection="outside"
	
	
	'ax1=lines(aintersect,1)
	'ay1=lines(aintersect,2)
	'ax2=lines(aintersect,4)
	'ay2=lines(aintersect,5)
	'bx1=lines(bintersect,1)
	'by1=lines(bintersect,2)
	'bx2=lines(bintersect,4)
	'by2=lines(bintersect,5)
	
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	
	det = A1*B2 - A2*B1
	If det = 0 Then
		intersection="parallel"
	Else
		fxm = (B2*C1 - B1*C2)/det
		'fxm=( (bx1-bx2) * ((ay2-ay1)*ax1+(ax1-ax2)*ay1) - (ax1-ax2) * ((by2-by1)*bx1+(bx1-bx2)*by1) ) / (det)
		fym = (A1*C2 - A2*C1)/det
		If Sqr((ax1-fxm)^2+(ay1-fym)^2)>Sqr((ax2-fxm)^2+(ay2-fym)^2) Then
			If Sqr((ax1-fxm)^2+(ay1-fym)^2)<=Sqr((ax1-ax2)^2+(ay1-ay2)^2) Then
				intersection="inside"
			Else
				intersection="outside"
			EndIf
		Else
			If Sqr((ax2-fxm)^2+(ay2-fym)^2)<=Sqr((ax1-ax2)^2+(ay1-ay2)^2) Then
				intersection="inside"
			Else
				intersection="outside"
			EndIf
		End If
	End If
	If intersection = "inside" Then
		intersection="outside"
		Select Case fxm
			Case ax1 To ax2,ax2 To ax1
				Select Case fym
					Case ay1 To ay2,ay2 To ay1
						intersection="inside"
				End Select
		End Select
	End If
	If intersection = "inside" Then
		intersection="outside"
		Select Case fxm
			Case bx1 To bx2,bx2 To bx1
				Select Case fym
					Case by1 To by2,by2 To by1
						intersection="inside"
				End Select
		End Select
	End If
End Sub
Sub calclinecircleintersection()
	Dim As Double ax1,ay1,ax2,ay2,cx1,cy1,cr1
	Dim As Double A1,B1,C1,det,dx1,dy1,fxm1,fxm2,fym1,fym2,aia
	Dim As Integer i
	ax1=lines(aintersect,1)
	ay1=lines(aintersect,2)
	ax2=lines(aintersect,4)
	ay2=lines(aintersect,5)
	cx1=circles(bintersect,1)
	cy1=circles(bintersect,2)
	cr1=circles(bintersect,4)
	dx1 = ax2 - ax1
	dy1 = ay2 - ay1
	
	A1 = dx1^2 + dy1^2
	B1 = 2 * (dx1 * (ax1 - cx1) + dy1 * (ay1 - cy1))
	C1 = (ax1 - cx1)^2 + (ay1 - cy1)^2 - cr1^2
	
	det = B1 * B1 - 4 * A1 * C1
	det=Abs(det)
	'one point
	fxm1 = ax1 + ((-B1 + Sqr(det)) / (2 * A1)) * dx1
	fym1 = ay1 + ((-B1 + Sqr(det)) / (2 * A1)) * dy1
	'other point
	fxm2 = ax1 + ((-B1 - Sqr(det)) / (2 * A1)) * dx1
	fym2 = ay1 + ((-B1 - Sqr(det)) / (2 * A1)) * dy1
	clic=0
	If sqr((cx1-fxm1)^2 + (cy1-fym1)^2) <= cr1+.0000001 Then
		Select Case fxm1
			Case ax1 To ax2, ax2 To ax1
				Select Case fym1
					Case ay1 To ay2, ay2 To ay1
						If circles(bintersect,9)=2 Then
							'Print "arc start=";circles(bintersect,6);" end=";circles(bintersect,7)
							'Is fxm1,fym1 in the arc
							xlength=fxm1-circles(bintersect,1)
							ylength=fym1-circles(bintersect,2)
							fixangle
							aia=PI*angle/180
							'Is aia between start and end angle
							If circles(bintersect,6)<circles(bintersect,7) then
								Select Case aia
									Case circles(bintersect,6) To circles(bintersect,7)
										clic=clic+1
										clifxm1=fxm1
										clifym1=fym1
								End Select
							Else
								Select Case aia
									Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
										clic=clic+1
										clifxm1=fxm1
										clifym1=fym1
								End Select
							End if
						Else
							clic=clic+1
							clifxm1=fxm1
							clifym1=fym1
						End If
						
				End Select
		End Select
	End If
	If sqr((cx1-fxm2)^2 + (cy1-fym2)^2) <= cr1+.0000001 Then
		Select Case fxm2
			Case ax1 To ax2, ax2 To ax1
				Select Case fym2
					Case ay1 To ay2, ay2 To ay1
						If circles(bintersect,9)=2 Then
							'Print "arc start=";circles(bintersect,6);" end=";circles(bintersect,7)
							'Is fxm2,fym2 in the arc
							xlength=fxm2-circles(bintersect,1)
							ylength=fym2-circles(bintersect,2)
							fixangle
							aia=PI*angle/180
							'Is aia between start and end angle
							If circles(bintersect,6)<circles(bintersect,7) then
								Select Case aia
									Case circles(bintersect,6) To circles(bintersect,7)
										clic=clic+1
										clifxm2=fxm2
										clifym2=fym2
										If clic=1 Then
											clifxm1=fxm2
											clifym1=fym2
										End If
								End Select
							Else
								Select Case aia
									Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
										clic=clic+1
										clifxm2=fxm2
										clifym2=fym2
										If clic=1 Then
											clifxm1=fxm2
											clifym1=fym2
										End If
								End Select
							End if
						Else
							clic=clic+1
							clifxm2=fxm2
							clifym2=fym2
							If clic=1 Then
								clifxm1=fxm2
								clifym1=fym2
							End If
						End If
						
				End Select
		End Select
	End If
	If clic>0 Then
		intersection="inside"
		if sqr((mousex-fxm1)^2 + (mousey-fym1)^2) < sqr((mousex-fxm2)^2 + (mousey-fym2)^2) Then
			fxm=fxm1
			fym=fym1
		Else
			fxm=fxm2
			fym=fym2
		End If
	Else
		intersection="outside"
	End If
End Sub
Sub calccirclecircleintersection()
	Dim As Double cx1,cy1,cr1,cx2,cy2,cr2
	Dim As Double A1,B1,C1,det,dx1,dy1,fxm1,fxm2,fym1,fym2
	Dim As Integer i
	cx1=circles(aintersect,1)
	cy1=circles(aintersect,2)
	cr1=circles(aintersect,4)
	cx2=circles(bintersect,1)
	cy2=circles(bintersect,2)
	cr2=circles(bintersect,4)
	dx1 = cx2 - cx1
	dy1 = cy2 - cy1

	intersection="outside"
	if sqr(dx1^2 + dy1^2) < (cr1 + cr2) And sqr(dx1^2 + dy1^2) > abs(cr1 - cr2) Then
		intersection="inside"
		A1 = (cr1^2 - cr2^2 + dx1^2 + dy1^2) / (2.0 * sqr(dx1^2 + dy1^2))
		B1 = sqr(abs(cr1^2 - A1^2))
		fxm1 = cx1 + (dx1 * A1/sqr(dx1^2 + dy1^2)) + -dy1 * (B1/sqr(dx1^2 + dy1^2))
		fxm2 = cx1 + (dx1 * A1/sqr(dx1^2 + dy1^2)) - (-dy1 * (B1/sqr(dx1^2 + dy1^2)))
		fym1 = cy1 + (dy1 * A1/sqr(dx1^2 + dy1^2)) + dx1 * (B1/sqr(dx1^2 + dy1^2))
		fym2 = cy1 + (dy1 * A1/sqr(dx1^2 + dy1^2)) - dx1 * (B1/sqr(dx1^2 + dy1^2))
		if sqr((mousex-fxm1)^2 + (mousey-fym1)^2) < sqr((mousex-fxm2)^2 + (mousey-fym2)^2) Then
			fxm=fxm1
			fym=fym1
		Else
			fxm=fxm2
			fym=fym2
		End If
		'ccic=?
		'if fxm1,fym1 is on circle / arc then ?
		Dim As Double aia
		ccic=0
		If circles(aintersect,9)=2 Then
			'the circle being trimmed is an arc
			'so check if fxm1&2 are in this circles arc
			'if they are in the arc then also check
			'if the other circle is an arc
			'if it is then make sure fxm1&2 are in it's arc
			xlength=fxm1-circles(aintersect,1)
			ylength=fym1-circles(aintersect,2)
			fixangle
			aia=PI*angle/180
			If circles(aintersect,6)<circles(aintersect,7) then
				Select Case aia
					Case circles(aintersect,6) To circles(aintersect,7)
						If circles(bintersect,9)=2 Then
							xlength=fxm1-circles(bintersect,1)
							ylength=fym1-circles(bintersect,2)
							fixangle
							aia=PI*angle/180
							If circles(bintersect,6)<circles(bintersect,7) then
								Select Case aia
									Case circles(bintersect,6) To circles(bintersect,7)
										ccic=ccic+1
										clifxm1=fxm1
										clifym1=fym1
								End Select
							Else
								Select Case aia
									Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
										ccic=ccic+1
										clifxm1=fxm1
										clifym1=fym1
								End Select
							End if
						Else
							ccic=ccic+1
							clifxm1=fxm1
							clifym1=fym1
						End If
				End Select
			Else
				Select Case aia
					Case circles(aintersect,6) To PI*2, 0 To circles(aintersect,7)
						If circles(bintersect,9)=2 Then
							xlength=fxm1-circles(bintersect,1)
							ylength=fym1-circles(bintersect,2)
							fixangle
							aia=PI*angle/180
							If circles(bintersect,6)<circles(bintersect,7) then
								Select Case aia
									Case circles(bintersect,6) To circles(bintersect,7)
										ccic=ccic+1
										clifxm1=fxm1
										clifym1=fym1
								End Select
							Else
								Select Case aia
									Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
										ccic=ccic+1
										clifxm1=fxm1
										clifym1=fym1
								End Select
							End if
						Else
							ccic=ccic+1
							clifxm1=fxm1
							clifym1=fym1
						End If
				End Select
			End if
			'fxm2
			xlength=fxm2-circles(aintersect,1)
			ylength=fym2-circles(aintersect,2)
			fixangle
			aia=PI*angle/180
			If circles(aintersect,6)<circles(aintersect,7) then
				Select Case aia
					Case circles(aintersect,6) To circles(aintersect,7)
						If circles(bintersect,9)=2 Then
							xlength=fxm2-circles(bintersect,1)
							ylength=fym2-circles(bintersect,2)
							fixangle
							aia=PI*angle/180
							If circles(bintersect,6)<circles(bintersect,7) then
								Select Case aia
									Case circles(bintersect,6) To circles(bintersect,7)
										ccic=ccic+1
										clifxm2=fxm2
										clifym2=fym2
								End Select
							Else
								Select Case aia
									Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
										ccic=ccic+1
										clifxm2=fxm2
										clifym2=fym2
								End Select
							End if
						Else
							ccic=ccic+1
							clifxm2=fxm2
							clifym2=fym2
						End If
				End Select
			Else
				Select Case aia
					Case circles(aintersect,6) To PI*2, 0 To circles(aintersect,7)
						If circles(bintersect,9)=2 Then
							xlength=fxm2-circles(bintersect,1)
							ylength=fym2-circles(bintersect,2)
							fixangle
							aia=PI*angle/180
							If circles(bintersect,6)<circles(bintersect,7) then
								Select Case aia
									Case circles(bintersect,6) To circles(bintersect,7)
										ccic=ccic+1
										clifxm2=fxm2
										clifym2=fym2
								End Select
							Else
								Select Case aia
									Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
										ccic=ccic+1
										clifxm2=fxm2
										clifym2=fym2
								End Select
							End if
						Else
							ccic=ccic+1
							clifxm2=fxm2
							clifym2=fym2
						End If
				End Select
			End if
		Else
			'the circle being trimmed is a full circle
			If circles(bintersect,9)=2 Then
				xlength=fxm1-circles(bintersect,1)
				ylength=fym1-circles(bintersect,2)
				fixangle
				aia=PI*angle/180
				If circles(bintersect,6)<circles(bintersect,7) then
					Select Case aia
						Case circles(bintersect,6) To circles(bintersect,7)
							ccic=ccic+1
							clifxm1=fxm1
							clifym1=fym1
					End Select
				Else
					Select Case aia
						Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
							ccic=ccic+1
							clifxm1=fxm1
							clifym1=fym1
					End Select
				End if
			Else
				ccic=ccic+1
				clifxm1=fxm1
				clifym1=fym1
			End If
			
			If circles(bintersect,9)=2 Then
				xlength=fxm2-circles(bintersect,1)
				ylength=fym2-circles(bintersect,2)
				fixangle
				aia=PI*angle/180
				If circles(bintersect,6)<circles(bintersect,7) then
					Select Case aia
						Case circles(bintersect,6) To circles(bintersect,7)
							ccic=ccic+1
							clifxm2=fxm2
							clifym2=fym2
							If ccic=1 Then
								clifxm1=fxm2
								clifym1=fym2
							End If
					End Select
				Else
					Select Case aia
						Case circles(bintersect,6) To PI*2, 0 To circles(bintersect,7)
							ccic=ccic+1
							clifxm2=fxm2
							clifym2=fym2
							If ccic=1 Then
								clifxm1=fxm2
								clifym1=fym2
							End If
					End Select
				End if
			Else
				ccic=ccic+1
				clifxm2=fxm2
				clifym2=fym2
				If ccic=1 Then
					clifxm1=fxm2
					clifym1=fym2
				End If
			End If

		EndIf
	EndIf
End Sub
Sub calclineellipseintersection()
	Dim As Double ax1,ay1,ax2,ay2,cx1,cy1,cr1,cr2,erot
	Dim As Double A1,B1,C1,det,dx1,dy1,fxm1,fxm2,fym1,fym2,erotx,eroty
	Dim As Integer i
	ax1=lines(aintersect,1)'ax1,ay1 are the x,y values of one end of line
	ay1=lines(aintersect,2)
	ax2=lines(aintersect,4)'ax2,ay2 are the x,y valuesof the other end of the line
	ay2=lines(aintersect,5)
	cx1=circles(bintersect,1)'cx1,cy1 are the values of the center of the ellipse
	cy1=circles(bintersect,2)
	cr1=circles(bintersect,4)'cr1,cr2 are the major & minor radius values of the ellipse
	cr2=circles(bintersect,8)
	erot=circles(bintersect,11)
	If erot<>0 Then 'if the ellipse is rotated then the line needs to be rotated
		x1p=ax1-cx1
		y1p=ay1-cy1
		x2p=ax2-cx1
		y2p=ay2-cy1
		ax1=x1p*cos(-erot*d2r) - y1p*sin(-erot*d2r)+cx1
		ay1=y1p*cos(-erot*d2r) + x1p*sin(-erot*d2r)+cy1
		ax2=x2p*cos(-erot*d2r) - y2p*sin(-erot*d2r)+cx1
		ay2=y2p*cos(-erot*d2r) + x2p*sin(-erot*d2r)+cy1
	EndIf
	dx1 = ax2 - ax1'dx1 is the X distance between the end points of a line
	dy1 = ay2 - ay1'dy1 is the Y distance between the end points of a line
	A1 = (1 / (dy1/dx1))^2 + (cr1 / cr2)^2
	B1 = (2 / (dy1/dx1)) * ((ax1-cx1) - (ay1-cy1) / (dy1/dx1))
	C1 = ((ax1-cx1) - (ay1-cy1) / (dy1/dx1))^2 - cr1^2
	det=B1^2 - 4 * A1 * C1
	intersection="outside"
	If dy1=0 Then
		If ay1-cy1<=cr2 Then
			intersection="inside"
			fym1=(ay1-cy1)
			fym2=(ay1-cy1)
			fxm1= cr1 * Sqr(1 - ((ay1-cy1) / cr2)^2)
			fxm2= -fxm1
		End if
	Else
		If det>0 Then
			intersection="inside"
			det=Abs(det)
			fym1 = (-B1 + Sqr(det)) / (2 * A1)
			fym2 = (-B1 - Sqr(det)) / (2 * A1)
			fxm1 = (fym1 - (ay1-cy1)) / (dy1/dx1) + (ax1-cx1)
			fxm2 = (fym2 - (ay1-cy1)) / (dy1/dx1) + (ax1-cx1)
		EndIf
	End If
	If intersection="inside" then
		fxm1=fxm1+cx1
		fxm2=fxm2+cx1
		fym1=fym1+cy1
		fym2=fym2+cy1
		'unrotate it if ellipse was rotated
		If erot<>0 Then
			x1p=fxm1-cx1
			y1p=fym1-cy1
			fxm1=x1p*cos(erot*d2r) - y1p*sin(erot*d2r)+cx1
			fym1=y1p*cos(erot*d2r) + x1p*sin(erot*d2r)+cy1
			x1p=fxm2-cx1
			y1p=fym2-cy1
			fxm2=x1p*cos(erot*d2r) - y1p*sin(erot*d2r)+cx1
			fym2=y1p*cos(erot*d2r) + x1p*sin(erot*d2r)+cy1
		EndIf
		'check to see which intersetion the mouse is nearest to
		if sqr((mousex-fxm1)^2 + (mousey-fym1)^2) < sqr((mousex-fxm2)^2 + (mousey-fym2)^2) Then
			fxm=fxm1
			fym=fym1
		Else
			fxm=fxm2
			fym=fym2
		End If
		'i think the code below is incase of an elliptical arc
		'Circle (fxm1,fym1),4
		'Circle (fxm2,fym2),4
		ax1=lines(aintersect,1)'ax1,ay1 are the x,y values of one end of line
		ay1=lines(aintersect,2)
		ax2=lines(aintersect,4)'ax2,ay2 are the x,y values of the of the other end of the line
		ay2=lines(aintersect,5)
		clic=0
		Dim As Double fam1,fam2,famx1,famy1,famx2,famy2
		famx1=cx1
		famy1=cy1
		famx2=fxm1
		famy2=fym1
		x1p=famx2-famx1
		y1p=(famy2-famy1)
		famx2=x1p*Cos(-erot*d2r) - y1p*Sin(-erot*d2r)+famx1
		famy2=y1p*Cos(-erot*d2r) + x1p*Sin(-erot*d2r)+famy1
		ylength=(famy2-famy1)*(cr1/cr2)
		xlength=famx2-famx1
		fixangle
		fam1=angle
		famx1=cx1
		famy1=cy1
		famx2=fxm2
		famy2=fym2
		x1p=famx2-famx1
		y1p=(famy2-famy1)
		famx2=x1p*Cos(-erot*d2r) - y1p*Sin(-erot*d2r)+famx1
		famy2=y1p*Cos(-erot*d2r) + x1p*Sin(-erot*d2r)+famy1
		ylength=(y2-y1)*(cr1/cr2)
		xlength=x2-x1
		fixangle
		fam2=angle
		'Print "fam1&2=";fam1;fam2
		Select Case fxm1
			Case ax1 To ax2, ax2 To ax1
				Select Case fym1
					Case ay1 To ay2, ay2 To ay1
						'if elliptical arc
						'if the angle (fxm1,fym1) to ellipse center in inside arc
						'taking into consideration erot
						If circles(bintersect,7) < circles(bintersect,6) Then
							Select Case fam1
								Case 0 To circles(bintersect,7), circles(bintersect,6) To 360
									clic=1
									clifxm1=fxm1
									clifym1=fym1
							End Select
						Else
							Select Case fam1
								Case circles(bintersect,6) To circles(bintersect,7), circles(bintersect,7) To circles(bintersect,6)
									clic=1
									clifxm1=fxm1
									clifym1=fym1
							End Select
						EndIf
				End Select
		End Select
'		Print "clic=";clic
		Select Case fxm2
			Case ax1 To ax2, ax2 To ax1
				Select Case fym2
					Case ay1 To ay2, ay2 To ay1
						'if elliptical arc
						'if the angle (fxm1,fym1) to ellipse center in inside arc
						'taking into consideration erot
						If circles(bintersect,7) < circles(bintersect,6) Then
							Select Case fam2
								Case 0 To circles(bintersect,7), circles(bintersect,6) To 360
									If clic=0 then
										clic=1
										clifxm1=fxm2
										clifym1=fym2
									Else
										clic=2
										clifxm2=fxm2
										clifym2=fym2
									End If
							End select
						Else
							Select Case fam2
								Case circles(bintersect,6) To circles(bintersect,7), circles(bintersect,7) To circles(bintersect,6)
									If clic=0 then
										clic=1
										clifxm1=fxm2
										clifym1=fym2
									Else
										clic=2
										clifxm2=fxm2
										clifym2=fym2
									End If
							End select
						EndIf
						
				End Select
		End Select
'		Print circles(bintersect,6),circles(bintersect,7)
'		Print fam1,fam2
	End If
End Sub
Sub calcellipseellipseintersection()
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	otd="ellipse"
	selcircle=aintersect
	calcnearestpoint
	perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
	perpangle+=90
	ax1=fxm
	ay1=fym
	ax2=fxm+cos(perpangle*d2r)*100
	ay2=fym+sin(perpangle*d2r)*100
	selcircle=bintersect
	calcnearestpoint
	perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
	perpangle+=90
	bx1=fxm
	by1=fym
	bx2=fxm+cos(perpangle*d2r)*100
	by2=fym+sin(perpangle*d2r)*100
	calclinelineintersection2(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)
	mousex=fxm
	mousey=fym
	selcircle=aintersect
	calcnearestpoint
	perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
	perpangle+=90
	ax1=fxm
	ay1=fym
	ax2=fxm+cos(perpangle*d2r)*100
	ay2=fym+sin(perpangle*d2r)*100
	selcircle=bintersect
	calcnearestpoint
	perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
	perpangle+=90
	bx1=fxm
	by1=fym
	bx2=fxm+cos(perpangle*d2r)*100
	by2=fym+sin(perpangle*d2r)*100
	calclinelineintersection2(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)
	mousex=mousextemp
	mousey=mouseytemp
	
End Sub
Sub calccircleellipseintersection()
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	selcircle=aintersect
	Select Case circles(selcircle,9)
		Case 1,2'circle or arc
			otd="circle"
			calcnearestpoint
			xlength=fxm-circles(selcircle,1)
			ylength=fym-circles(selcircle,2)
			fixperpangle
			perpangle+=90
			ax1=fxm
			ay1=fym
			ax2=fxm+cos(perpangle*d2r)*100
			ay2=fym+sin(perpangle*d2r)*100
		Case 3,4'ellipse or elliptical arc
			otd="ellipse"
			calcnearestpoint
			perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
			perpangle+=90
			ax1=fxm
			ay1=fym
			ax2=fxm+cos(perpangle*d2r)*100
			ay2=fym+sin(perpangle*d2r)*100
	End Select
	selcircle=bintersect
	Select Case circles(selcircle,9)
		Case 1,2'circle or arc
			otd="circle"
			calcnearestpoint
			xlength=fxm-circles(selcircle,1)
			ylength=fym-circles(selcircle,2)
			fixperpangle
			perpangle+=90
			bx1=fxm
			by1=fym
			bx2=fxm+cos(perpangle*d2r)*100
			by2=fym+sin(perpangle*d2r)*100
		Case 3,4'ellipse or elliptical arc
			otd="ellipse"
			calcnearestpoint
			perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
			perpangle+=90
			bx1=fxm
			by1=fym
			bx2=fxm+cos(perpangle*d2r)*100
			by2=fym+sin(perpangle*d2r)*100
	End Select
	calclinelineintersection2(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)
	mousex=fxm
	mousey=fym
	selcircle=aintersect
	Select Case circles(selcircle,9)
		Case 1,2'circle or arc
			otd="circle"
			calcnearestpoint
			xlength=fxm-circles(selcircle,1)
			ylength=fym-circles(selcircle,2)
			fixperpangle
			perpangle+=90
			ax1=fxm
			ay1=fym
			ax2=fxm+cos(perpangle*d2r)*100
			ay2=fym+sin(perpangle*d2r)*100
		Case 3,4'ellipse or elliptical arc
			otd="ellipse"
			calcnearestpoint
			perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
			perpangle+=90
			ax1=fxm
			ay1=fym
			ax2=fxm+cos(perpangle*d2r)*100
			ay2=fym+sin(perpangle*d2r)*100
	End Select
	selcircle=bintersect
	Select Case circles(selcircle,9)
		Case 1,2'circle or arc
			otd="circle"
			calcnearestpoint
			xlength=fxm-circles(selcircle,1)
			ylength=fym-circles(selcircle,2)
			fixperpangle
			perpangle+=90
			bx1=fxm
			by1=fym
			bx2=fxm+cos(perpangle*d2r)*100
			by2=fym+sin(perpangle*d2r)*100
		Case 3,4'ellipse or elliptical arc
			otd="ellipse"
			calcnearestpoint
			perpangle=calcellipsenorm(circles(selcircle,1),circles(selcircle,2),circles(selcircle,3),circles(selcircle,4),Int(circles(selcircle,5)),circles(selcircle,6),circles(selcircle,7),circles(selcircle,8),circles(selcircle,11))
			perpangle+=90
			bx1=fxm
			by1=fym
			bx2=fxm+cos(perpangle*d2r)*100
			by2=fym+sin(perpangle*d2r)*100
	End Select
	calclinelineintersection2(ax1,ay1,ax2,ay2,bx1,by1,bx2,by2)
	mousex=mousextemp
	mousey=mouseytemp
	
End Sub
Sub extendobjects
	'Print "extend"
End Sub
Sub setdimensiondown()
	tempstring=dimvaluestring
	Dim As Integer i
	dimensioning=TRUE
	select case modify
		Case 71'dimension
			Dim As Double tempdx1,tempdy1,tempdx2,tempdy2
			tempdx1=dimensionxy(4,1)
			tempdy1=dimensionxy(4,2)
			tempdx2=dimensionxy(5,1)
			tempdy2=dimensionxy(5,2)
			'calcperp from mousex,y to line(dimx1,y1)-(dimx2,y2)
			memmanageline
			
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(1,1)
			lines(linec,5)=dimensionxy(1,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength-dimleaderspacing
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			dimensionxy(1,1)=fx
			dimensionxy(1,2)=fy
			
			lines(linec,1)=dimensionxy(5,1)
			lines(linec,2)=dimensionxy(5,2)
			lines(linec,4)=dimensionxy(2,1)
			lines(linec,5)=dimensionxy(2,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength-dimleaderspacing
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			dimensionxy(2,1)=fx
			dimensionxy(2,2)=fy
			''''''''''
			lines(linec,1)=dimensionxy(1,1)
			lines(linec,2)=dimensionxy(1,2)
			lines(linec,4)=dimensionxy(4,1)
			lines(linec,5)=dimensionxy(4,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength+dimarrowsize/2
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			altosellength'returns fx,fy
			dimensionxy(4,1)=fx
			dimensionxy(4,2)=fy
			
			lines(linec,1)=dimensionxy(2,1)
			lines(linec,2)=dimensionxy(2,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength+dimarrowsize/2
			x1=dimensionxy(2,1)
			y1=dimensionxy(2,2)
			altosellength'returns fx,fy
			dimensionxy(5,1)=fx
			dimensionxy(5,2)=fy
			linec=linec-1
			'''''''''''
			'left leg
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			z1=0
			x2=dimensionxy(4,1)
			y2=dimensionxy(4,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'right leg
			x1=dimensionxy(2,1)
			y1=dimensionxy(2,2)
			z1=0
			x2=dimensionxy(5,1)
			y2=dimensionxy(5,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			''''''''''
			'line between legs
			memmanageline
			dimensionxy(4,1)=tempdx1
			dimensionxy(4,2)=tempdy1
			dimensionxy(5,1)=tempdx2
			dimensionxy(5,2)=tempdy2
			'making room for the arrow
			'shorten up the left side
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength-dimarrowsize
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			dimensionxy(5,1)=fx
			dimensionxy(5,2)=fy
			'shorten up the right side
			lines(linec,1)=dimensionxy(5,1)
			lines(linec,2)=dimensionxy(5,2)
			lines(linec,4)=dimensionxy(4,1)
			lines(linec,5)=dimensionxy(4,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength-dimarrowsize
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			dimensionxy(4,1)=fx
			dimensionxy(4,2)=fy
			linec=linec-1
			'create line between legs
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			z1=0
			x2=dimensionxy(5,1)
			y2=dimensionxy(5,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			''''''''''
			
			'define arrows
			'define left arrow
			memmanageline
			Dim As Double tempfx1,tempfx2,tempfy1,tempfy2
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			selangle=selangle+90
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			tempfx1=fx
			tempfy1=fy
			selangle=selangle+180
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			tempfx2=fx
			tempfy2=fy
			linec=linec-1
			'create left arrow
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempfx2
			y2=tempfy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempdx1
			y2=tempdy1
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx2
			y1=tempfy2
			z1=0
			x2=tempdx1
			y2=tempdy1
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'define right arrow
			memmanageline
			'Dim As Double tempfx1,tempfx2,tempfy1,tempfy2
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			selangle=selangle+90
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			tempfx1=fx
			tempfy1=fy
			selangle=selangle+180
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			tempfx2=fx
			tempfy2=fy
			linec=linec-1
			
			'create right arrow
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempfx2
			y2=tempfy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempdx2
			y2=tempdy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx2
			y1=tempfy2
			z1=0
			x2=tempdx2
			y2=tempdy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'tempdouble=calcd(dimensionxy(1,1),dimensionxy(1,2),dimensionxy(1,3),dimensionxy(2,1),dimensionxy(2,2),dimensionxy(2,3))
			'tempstring=Str(tempdouble)
			plottextstring(tempstring,CDbl(mousex),CDbl(mousey)+dimleadertextoffsetspacing,0)
		Case 72'dimension X		
			'arrows are placed on the inside or between the
			'dimension legs when user selects the to points to
			'dimension from left to right
			'if user selects two points from right to left
			'then arrows are place on the outside of the legs
			If mousey < dimensionxy(1,2) Then
				dimensionxy(1,2)=dimensionxy(1,2)-dimleaderspacing
				dimensionxy(2,2)=dimensionxy(2,2)-dimleaderspacing
				y2=mousey-dimarrowsize/2
			Else
				dimensionxy(1,2)=dimensionxy(1,2)+dimleaderspacing
				dimensionxy(2,2)=dimensionxy(2,2)+dimleaderspacing
				y2=mousey+dimarrowsize/2
			EndIf
			'left leg
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			z1=0
			x2=dimensionxy(1,1)
			'y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'right leg
			x1=dimensionxy(2,1)
			y1=dimensionxy(2,2)
			z1=0
			x2=dimensionxy(2,1)
			'y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'line between legs
			x1=dimensionxy(1,1)+dimarrowsize
			y1=mousey
			z1=0
			x2=dimensionxy(2,1)-dimarrowsize
			y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'left arrow
			x1=dimensionxy(1,1)+dimarrowsize
			y1=mousey-dimarrowsize/2
			z1=0
			x2=dimensionxy(1,1)+dimarrowsize
			y2=mousey+dimarrowsize/2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(1,1)+dimarrowsize
			y1=mousey-dimarrowsize/2
			z1=0
			x2=dimensionxy(1,1)
			y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(1,1)+dimarrowsize
			y1=mousey+dimarrowsize/2
			z1=0
			x2=dimensionxy(1,1)
			y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'right arrow
			x1=dimensionxy(2,1)-dimarrowsize
			y1=mousey-dimarrowsize/2
			z1=0
			x2=dimensionxy(2,1)-dimarrowsize
			y2=mousey+dimarrowsize/2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(2,1)-dimarrowsize
			y1=mousey-dimarrowsize/2
			z1=0
			x2=dimensionxy(2,1)
			y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(2,1)-dimarrowsize
			y1=mousey+dimarrowsize/2
			z1=0
			x2=dimensionxy(2,1)
			y2=mousey
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'tempdouble=dimensionxy(1,1)-dimensionxy(2,1)'calcd(dimensionxy(1,1),dimensionxy(1,2),dimensionxy(1,3),dimensionxy(2,1),dimensionxy(2,2),dimensionxy(2,3))
			'tempstring=Str(tempdouble)
			plottextstring(tempstring,CDbl(mousex),CDbl(mousey)+dimleadertextoffsetspacing,0)
		Case 73'dimension Y
			'arrows are placed on the inside of the dimension legs
			'when user selects two point from top to bottom
			'and on the outside of the legs when selected from
			'bottom up
			If mousex < dimensionxy(1,1) Then
				dimensionxy(1,1)=dimensionxy(1,1)-dimleaderspacing
				dimensionxy(2,1)=dimensionxy(2,1)-dimleaderspacing
				x2=mousex-dimarrowsize/2
			Else
				dimensionxy(1,1)=dimensionxy(1,1)+dimleaderspacing
				dimensionxy(2,1)=dimensionxy(2,1)+dimleaderspacing
				x2=mousex+dimarrowsize/2
			EndIf
			'top leg
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			z1=0
			'x2=mousex
			y2=dimensionxy(1,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'bottom leg
			x1=dimensionxy(2,1)
			y1=dimensionxy(2,2)
			z1=0
			'x2=mousex
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'line between legs
			x1=mousex
			y1=dimensionxy(1,2)-dimarrowsize
			z1=0
			x2=mousex
			y2=dimensionxy(2,2)+dimarrowsize
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'top arrow
			x1=mousex-dimarrowsize/2
			y1=dimensionxy(1,2)-dimarrowsize
			z1=0
			x2=mousex+dimarrowsize/2
			y2=dimensionxy(1,2)-dimarrowsize
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=mousex-dimarrowsize/2
			y1=dimensionxy(1,2)-dimarrowsize
			z1=0
			x2=mousex
			y2=dimensionxy(1,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=mousex+dimarrowsize/2
			y1=dimensionxy(1,2)-dimarrowsize
			z1=0
			x2=mousex
			y2=dimensionxy(1,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'bottom arrow
			x1=mousex-dimarrowsize/2
			y1=dimensionxy(2,2)+dimarrowsize
			z1=0
			x2=mousex+dimarrowsize/2
			y2=dimensionxy(2,2)+dimarrowsize
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=mousex-dimarrowsize/2
			y1=dimensionxy(2,2)+dimarrowsize
			z1=0
			x2=mousex
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=mousex+dimarrowsize/2
			y1=dimensionxy(2,2)+dimarrowsize
			z1=0
			x2=mousex
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'tempdouble=dimensionxy(1,2)-dimensionxy(2,2)'calcd(dimensionxy(1,1),dimensionxy(1,2),dimensionxy(1,3),dimensionxy(2,1),dimensionxy(2,2),dimensionxy(2,3))
			'tempstring=Str(tempdouble)
			plottextstring(tempstring,CDbl(mousex)+dimleadertextoffsetspacing,CDbl(mousey),0)
		Case 74'radius - set dim down
			x1=dimensionxy(1,1)-10
			y1=dimensionxy(1,2)
			z1=0
			x2=dimensionxy(1,1)+10
			y2=dimensionxy(1,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)-10
			z1=0
			x2=dimensionxy(1,1)
			y2=dimensionxy(1,2)+10
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			z1=0
			x2=dimensionxy(3,1)
			y2=dimensionxy(3,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			z1=0
			x2=dimensionxy(5,1)
			y2=dimensionxy(5,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			z1=0
			x2=dimensionxy(2,1)
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			z1=0
			x2=dimensionxy(2,1)
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=dimensionxy(2,1)
			y1=dimensionxy(2,2)
			z1=0
			x2=dimensionxy(6,1)
			y2=dimensionxy(6,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'tempdouble=circles(tempselcircle,4)
			'tempstring=Str(tempdouble)
			plottextstring(tempstring,CDbl(mousex),CDbl(mousey),0)
		Case 75
			'dimension diameter - set dim down
			memmanageline
			lines(linec,1)=dimensionxy(1,1)
			lines(linec,2)=dimensionxy(1,2)
			lines(linec,4)=mousex
			lines(linec,5)=mousey
			selline=linec
			linec=linec-1
			calcselangle
			'calcsellength
			sellength=circles(tempselcircle,4)
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			altosellength'returns fx,fy
			'this is the second end of the diameter dimension line
			dimensionxy(5,1)=fx
			dimensionxy(5,2)=fy
			'line(dimensionxy(1,1),dimensionxy(1,2))-(dimensionxy(2,1),dimensionxy(2,2))
			selangle=selangle+180
			'calcsellength
			sellength=circles(tempselcircle,4)
			x1=dimensionxy(1,1)
			y1=dimensionxy(1,2)
			altosellength'returns fx,fy
			'this is the first end of the diameter dimension line
			dimensionxy(4,1)=fx
			dimensionxy(4,2)=fy
			'dim array 4 & 5 is used to calc arrow positions
			Dim As Double tempdx1,tempdy1,tempdx2,tempdy2
			tempdx1=dimensionxy(4,1)
			tempdy1=dimensionxy(4,2)
			tempdx2=dimensionxy(5,1)
			tempdy2=dimensionxy(5,2)
			memmanageline
			'making room for the arrow
			'shorten up the left side
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength-dimarrowsize
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			dimensionxy(5,1)=fx
			dimensionxy(5,2)=fy
			'shorten up the right side
			lines(linec,1)=dimensionxy(5,1)
			lines(linec,2)=dimensionxy(5,2)
			lines(linec,4)=dimensionxy(4,1)
			lines(linec,5)=dimensionxy(4,2)
			selline=linec
			calcselangle
			calcsellength
			sellength=sellength-dimarrowsize
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			dimensionxy(4,1)=fx
			dimensionxy(4,2)=fy
			linec=linec-1
			'show line between legs
			'Line (dimensionxy(4,1),dimensionxy(4,2))-(dimensionxy(5,1),dimensionxy(5,2))
			'create line between legs
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			z1=0
			x2=dimensionxy(5,1)
			y2=dimensionxy(5,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			''''''''''
			
			'define arrows
			'define left arrow
			memmanageline
			Dim As Double tempfx1,tempfx2,tempfy1,tempfy2
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			selangle=selangle+90
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			tempfx1=fx
			tempfy1=fy
			selangle=selangle+180
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			altosellength'returns fx,fy
			tempfx2=fx
			tempfy2=fy
			linec=linec-1
			'show left arrow
			'Line (tempfx1,tempfy1)-(tempfx2,tempfy2)
			'Line (tempfx1,tempfy1)-(tempdx1,tempdy1)
			'Line (tempfx2,tempfy2)-(tempdx1,tempdy1)
			'create left arrow
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempfx2
			y2=tempfy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempdx1
			y2=tempdy1
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx2
			y1=tempfy2
			z1=0
			x2=tempdx1
			y2=tempdy1
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'define right arrow
			memmanageline
			'Dim As Double tempfx1,tempfx2,tempfy1,tempfy2
			lines(linec,1)=dimensionxy(4,1)
			lines(linec,2)=dimensionxy(4,2)
			lines(linec,4)=dimensionxy(5,1)
			lines(linec,5)=dimensionxy(5,2)
			selline=linec
			calcselangle
			selangle=selangle+90
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			tempfx1=fx
			tempfy1=fy
			selangle=selangle+180
			If selangle>360 Then selangle=selangle-360
			sellength=dimarrowsize/2
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			altosellength'returns fx,fy
			tempfx2=fx
			tempfy2=fy
			linec=linec-1
			'show right arrow
			'Line (tempfx1,tempfy1)-(tempfx2,tempfy2)
			'Line (tempfx1,tempfy1)-(tempdx2,tempdy2)
			'Line (tempfx2,tempfy2)-(tempdx2,tempdy2)
			'create right arrow
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempfx2
			y2=tempfy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx1
			y1=tempfy1
			z1=0
			x2=tempdx2
			y2=tempdy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			x1=tempfx2
			y1=tempfy2
			z1=0
			x2=tempdx2
			y2=tempdy2
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
'			dimvalue=circles(tempselcircle,4)*2
'			'tempstring=Str(tempdouble)
'			adjustprecision
'			Draw String (mousex,mousey),tempstring
			'Dim As Integer templinec,tempcirclec
			'templinec=linec+1
			'tempcirclec=circlec+1
			plottextstring(tempstring,CDbl(mousex),CDbl(mousey),0)
		Case 76
			memmanagecircle
			circles(circlec,1)=x1
			circles(circlec,2)=y1
		
			circles(circlec,4)=radius
			circles(circlec,5)=lc
			circles(circlec,6)=arcstart
			circles(circlec,7)=arcend
			circles(circlec,9)=2
			circles(circlec,10)=blockc+1
			circles(circlec,12)=blockc+1
			'arcstart arrow
			'Line (dimensionxy(3,1) ,dimensionxy(3,2) )-(dimensionxy(4,1) ,dimensionxy(4,2) )
			x1=dimensionxy(3,1)
			y1=dimensionxy(3,2)
			z1=0
			x2=dimensionxy(4,1)
			y2=dimensionxy(4,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'Line (dimensionxy(3,1) ,dimensionxy(3,2) )-(dimensionxy(1,1) ,dimensionxy(1,2) )
			x1=dimensionxy(3,1)
			y1=dimensionxy(3,2)
			z1=0
			x2=dimensionxy(1,1)
			y2=dimensionxy(1,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'Line (dimensionxy(4,1) ,dimensionxy(4,2) )-(dimensionxy(1,1) ,dimensionxy(1,2) )
			x1=dimensionxy(4,1)
			y1=dimensionxy(4,2)
			z1=0
			x2=dimensionxy(1,1)
			y2=dimensionxy(1,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			
			'arcend arrow
			'Line (dimensionxy(5,1) ,dimensionxy(5,2) )-(dimensionxy(6,1) ,dimensionxy(6,2) )
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			z1=0
			x2=dimensionxy(6,1)
			y2=dimensionxy(6,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'Line (dimensionxy(5,1) ,dimensionxy(5,2) )-(dimensionxy(2,1) ,dimensionxy(2,2) )
			x1=dimensionxy(5,1)
			y1=dimensionxy(5,2)
			z1=0
			x2=dimensionxy(2,1)
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'Line (dimensionxy(6,1) ,dimensionxy(6,2) )-(dimensionxy(2,1) ,dimensionxy(2,2) )
			x1=dimensionxy(6,1)
			y1=dimensionxy(6,2)
			z1=0
			x2=dimensionxy(2,1)
			y2=dimensionxy(2,2)
			z2=0
			createnewmodlines
			lines(linec,8)=blockc+1
			lines(linec,9)=blockc+1
			'draw angle text
			'Dim As Integer templinec,tempcirclec
			'templinec=linec+1
			'tempcirclec=circlec+1
			'tempstring=Str(dimvalue)
			plottextstring(tempstring,CDbl(mousex),CDbl(mousey),0)
	End Select
	'screenset 0,0:view:window
	selbutton=modify
	turnbuttonoff
	modify=31
	selbutton=modify
	groupexists=TRUE
	turnbuttonon
	'buttonson(31)=TRUE
	modifying=FALSE
	modifyx1=mousex
	modifyy1=mousey
	dimensioning=FALSE
	drawing=false
	drawmode=FALSE
	redraw
	'showgroups

End Sub
sub importblockdxf(bfn As String)
	Dim As Integer i
	Dim As String itemstring
'	c=0
'	circlec=0
'	redim lines(100,8) as double
	'screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	open bfn for input as #1
	'If Err<>0 Then Exit Sub
	itemstring=""

	do while not eof(1)
		'load arrays
		line input #1, tempstring
		Select Case tempstring
			Case "  0"
				itemstring=""
			Case "LINE"
				memmanageline
				itemstring="LINE"
				lines(linec,7)=15
				lines(linec,8)=(blockc+1)*-1
			Case "CIRCLE"
				memmanagecircle
				circles(circlec,9)=1
				itemstring="CIRCLE"
				circles(circlec,5)=15
				circles(circlec,10)=(blockc+1)*-1
			Case "ARC"
				memmanagecircle
				circles(circlec,9)=2
				itemstring="ARC"
				circles(circlec,5)=15
				circles(circlec,10)=(blockc+1)*-1
			Case "ELLIPSE"
				memmanagecircle
				itemstring="ELLIPSE"
				circles(circlec,5)=15
				circles(circlec,10)=(blockc+1)*-1
		End Select
		Select Case itemstring
			Case "LINE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							lines(linec,7)=val(mid$(tempstring,8))
						Else
							lines(linec,7)=Val(tempstring) Mod 15
							If lines(linec,7)=0 Then lines(linec,7)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						lines(linec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						lines(linec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						lines(linec,3)=val(tempstring)
					Case " 11"
						line input #1, tempstring
						lines(linec,4)=val(tempstring)
					Case " 21"
						line input #1, tempstring
						lines(linec,5)=val(tempstring)
					Case " 31"
						line input #1, tempstring
						lines(linec,6)=val(tempstring)
				End Select
			Case "CIRCLE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 40"
						line input #1, tempstring
						circles(circlec,4)=val(tempstring)
				End Select
			Case "ARC"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 40"
						line input #1, tempstring
						circles(circlec,4)=val(tempstring)
					Case " 50"
						line input #1, tempstring
						circles(circlec,6)=val(tempstring)*d2r
					Case " 51"
						line input #1, tempstring
						circles(circlec,7)=val(tempstring)*d2r
				End Select
			Case "ELLIPSE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 11"
						line input #1, tempstring
						x2=val(tempstring)
					Case " 21"
						line input #1, tempstring
						x1=circles(circlec,1)
						y1=circles(circlec,2)
						y2=val(tempstring)
						x2=x1+x2
						y2=y1+y2
						circles(circlec,4)=(sqr((x1-x2)^2 + (y1-y2)^2))
					Case " 31"
						line input #1, tempstring
					Case " 40"
						line input #1, tempstring
						circles(circlec,8)=circles(circlec,4)*val(tempstring)
						trackangle
						circles(circlec,11)=angle
					Case " 41"
						line input #1, tempstring
						circles(circlec,6)=val(tempstring)*180/pi
					Case " 42"
						line input #1, tempstring
						circles(circlec,7)=val(tempstring)*180/pi
						If circles(circlec,6)=0 And circles(circlec,7)=360 Then
							circles(circlec,9)=3
						Else
							circles(circlec,9)=4
						EndIf
				End Select
		End Select
	loop
	close #1
End Sub
sub createanchar(anchar As Integer ,ancharx As Double ,anchary As Double)
	Dim As Integer i
	tempint=linec
	ancharwidth=0
	for i = 1 to tempint
		if lines(i,8)=anchar*-1-1 then
			x1=lines(i,1)+ancharx
			y1=lines(i,2)+anchary
			z1=lines(i,3)'+ancharz
			x2=lines(i,4)+ancharx
			y2=lines(i,5)+anchary
			z2=lines(i,6)'+ancharz
			createnewmodlines
			lines(linec,8)=blockc
			lines(linec,9)=blockc
			'to handle character spacing
			'find the greatest x value and assign to global variable
			If lines(i,1)>ancharwidth Then ancharwidth=lines(i,1)
			If lines(i,4)>ancharwidth Then ancharwidth=lines(i,4)
		end if
	Next
	tempint=circlec
	for i = 1 to tempint
		If circles(i,10)=anchar*-1-1 Then
			x1=circles(i,1)+ancharx
			y1=circles(i,2)+anchary
			z1=circles(i,3)'+ancharx
			radius=circles(i,4)
			arcstart=circles(i,6)
			arcend=circles(i,7)
			aspect=circles(i,8)
			erotation=circles(i,11)
			createnewmodcircles(Int(circles(i,9)))
			circles(circlec,10)=blockc
			circles(circlec,12)=blockc
		EndIf
	Next
	'initlinedraw
end sub
Sub plottextstring(ptstring As String, xoffset As Double, yoffset As Double, fontsize as Integer)
	Dim As Integer i,j,k
	Dim As Integer templinec,tempcirclec
	Dim As Double tempxoffset,tempyoffset
	Dim As Integer newblockcreated
	Dim As BOOLEAN blockextentsset
	Dim As Integer blockentityc

	If dimensioning=TRUE Then
		blockc=blockc+2
		ReDim Preserve blocknames(blockc)
		ReDim Preserve blockoffsets(blockc,7)
		ReDim Preserve blockstatus(blockc)
		blockc=blockc-1
		blocknames(blockc)="fbcdim"+LTrim(Str(blockc))
		'dimensioning=FALSE
		'create a block of the dim lines / arrows
		
		
		
		blockextentsset=FALSE
		blockentityc=0
		For i = 1 To linec
			If lines(i,9)=blockc Then
				blockentityc=blockentityc+1
				If blockextentsset=FALSE Then
					blockextentsset=TRUE
					extentsx1=lines(i,1)
					extentsy1=lines(i,2)
					extentsx2=lines(i,1)
					extentsy2=lines(i,2)
					If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)
					If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)
					If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)
					If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)
				Else
					If lines(i,1)<extentsx1 Then extentsx1=lines(i,1)
					If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)
					If lines(i,2)<extentsy1 Then extentsy1=lines(i,2)
					If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)
					If lines(i,1)>extentsx2 Then extentsx2=lines(i,1)
					If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)
					If lines(i,2)>extentsy2 Then extentsy2=lines(i,2)
					If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)
				EndIf
			EndIf
		Next
		For i = 1 To circlec
			If circles(i,12)=blockc Then
				blockentityc=blockentityc+1
				If blockextentsset=FALSE Then
					blockextentsset=TRUE
					extentsx1=circles(i,1)-circles(i,4)
					extentsx2=circles(i,1)+circles(i,4)
					If circles(i,1)-circles(i,8)<extentsx1 Then extentsx1=circles(i,1)-circles(i,8)
					If circles(i,1)+circles(i,8)>extentsx2 Then extentsx2=circles(i,1)+circles(i,8)
					extentsy1=circles(i,2)-circles(i,4)
					extentsy2=circles(i,2)+circles(i,4)
					If circles(i,2)-circles(i,8)<extentsy1 Then extentsy1=circles(i,2)-circles(i,8)
					If circles(i,2)+circles(i,8)>extentsy2 Then extentsy2=circles(i,2)+circles(i,8)
				Else
					If circles(i,1)-circles(i,4)<extentsx1 Then extentsx1=circles(i,1)-circles(i,4)
					If circles(i,1)-circles(i,8)<extentsx1 Then extentsx1=circles(i,1)-circles(i,8)
					If circles(i,2)-circles(i,4)<extentsy1 Then extentsy1=circles(i,2)-circles(i,4)
					If circles(i,2)-circles(i,8)<extentsy1 Then extentsy1=circles(i,2)-circles(i,8)
					If circles(i,1)+circles(i,4)>extentsx2 Then extentsx2=circles(i,1)+circles(i,4)
					If circles(i,1)+circles(i,8)>extentsx2 Then extentsx2=circles(i,1)+circles(i,8)
					If circles(i,2)+circles(i,4)>extentsy2 Then extentsy2=circles(i,2)+circles(i,4)
					If circles(i,2)+circles(i,8)>extentsy2 Then extentsy2=circles(i,2)+circles(i,8)
				End If
			End If
		Next
		
		blockoffsets(blockc,1)=0
		blockoffsets(blockc,2)=0
		blockoffsets(blockc,3)=0
		
		modifyx1=extentsx1
		modifyy1=extentsy1
		modifyx2=0
		modifyy2=0
		movedxfblock(blockc)
		blockstatus(blockc)=-1
		For i = 1 To linec
			If lines(i,9)=blockc Then lines(i,8)=-1
		Next
		For i = 1 To circlec
			If circles(i,12)=blockc Then circles(i,10)=-1
		Next
		
		blockc=blockc+1
		i=blockc-1
		tempint=linec
		For j=1 To tempint
			If lines(j,9)=i Then
				memmanageline
				For k=1 To 9
					lines(linec,k)=lines(j,k)
				Next
				lines(linec,8)=0
				lines(linec,9)=blockc
			End If
		Next
		tempint=circlec
		For j=1 To tempint
			If circles(j,12)=i Then
				memmanagecircle
				For k=1 To 12
					circles(circlec,k)=circles(j,k)
				Next
				circles(circlec,10)=0
				circles(circlec,12)=blockc
			EndIf
		Next

		modifyx1=0
		modifyy1=0
		modifyx2=extentsx1
		modifyy2=extentsy1
		movedxfblock(blockc)
		blocknames(blockc)=blocknames(blockc-1)
		blockoffsets(blockc,1)=extentsx1
		blockoffsets(blockc,2)=extentsy1
		blockoffsets(blockc,3)=0
		blockoffsets(blockc,4)=1
		blockoffsets(blockc,5)=1
		blockoffsets(blockc,6)=1
		blockoffsets(blockc,7)=0
		
		'now continue with creating the text for the dim
		blockc=blockc+2
		ReDim Preserve blocknames(blockc)
		ReDim Preserve blockoffsets(blockc,7)
		ReDim Preserve blockstatus(blockc)
		blockc=blockc-1
		blocknames(blockc)="fbctxt"+LTrim(Str(blockc))
	Else
		blockc=blockc+2
		ReDim Preserve blocknames(blockc)
		ReDim Preserve blockoffsets(blockc,7)
		ReDim Preserve blockstatus(blockc)
		blockc=blockc-1
		blocknames(blockc)="fbctxt"+LTrim(Str(blockc))
	EndIf
	
	templinec=linec+1
	tempcirclec=circlec+1
	tempxoffset=xoffset
	tempyoffset=yoffset
	ptstring=UCase(ptstring)
	newblockcreated=0
	For i = 1 To Len(ptstring)
		Select Case Asc(Mid(ptstring,i,1))
			Case 65 To 90'A-Z
				newblockcreated=1
				createanchar(Asc(Mid(ptstring,i,1))-64,xoffset,yoffset)
				xoffset=xoffset+ancharwidth + 50'50 is space between char
			Case 48 To 57'0-9
				newblockcreated=1
				createanchar(Asc(Mid(ptstring,i,1))-47+26,xoffset,yoffset)
				xoffset=xoffset+ancharwidth + 50'50 is space between char
			Case 46'period
				newblockcreated=1
				createanchar(37,xoffset,yoffset)
				xoffset=xoffset+ancharwidth + 50'50 is space between char 
			Case 32'space
				xoffset=xoffset+100'100 is space of a space char
		End Select
	Next
	If newblockcreated=0 Then
		blockc=blockc-1
	else
		If fontsize=0 Then fontsize=textsize
		For i = templinec To linec
			scaleit("LINE",i,fontsize*.015625,fontsize*.015625,fontsize*.015625,tempxoffset,tempyoffset)
		Next
		For i = tempcirclec To circlec
			scaleit("CIRCLE",i,fontsize*.015625,fontsize*.015625,fontsize*.015625,tempxoffset,tempyoffset)
		Next
		
		blockextentsset=FALSE
		blockentityc=0
		'savedxfblockonly=TRUE
		'savedxfblockonlyi=blockc
		For i = 1 To linec
			If lines(i,9)=blockc Then
				blockentityc=blockentityc+1
				If blockextentsset=FALSE Then
					blockextentsset=TRUE
					extentsx1=lines(i,1)
					extentsy1=lines(i,2)
					extentsx2=lines(i,1)
					extentsy2=lines(i,2)
					If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)
					If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)
					If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)
					If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)
				Else
					If lines(i,1)<extentsx1 Then extentsx1=lines(i,1)
					If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)
					If lines(i,2)<extentsy1 Then extentsy1=lines(i,2)
					If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)
					If lines(i,1)>extentsx2 Then extentsx2=lines(i,1)
					If lines(i,4)>extentsx2 Then extentsx2=lines(i,4)
					If lines(i,2)>extentsy2 Then extentsy2=lines(i,2)
					If lines(i,5)>extentsy2 Then extentsy2=lines(i,5)
				EndIf
			EndIf
		Next
		For i = 1 To circlec
			If circles(i,12)=blockc Then
				blockentityc=blockentityc+1
				Select Case circles(i,9)
					Case 1
						If blockextentsset=FALSE Then
							blockextentsset=TRUE
							extentsx1=circles(i,1)-circles(i,4)
							extentsx2=circles(i,1)+circles(i,4)
							extentsy1=circles(i,2)-circles(i,4)
							extentsy2=circles(i,2)+circles(i,4)
						Else
							If circles(i,1)-circles(i,4)<extentsx1 Then extentsx1=circles(i,1)-circles(i,4)
							If circles(i,2)-circles(i,4)<extentsy1 Then extentsy1=circles(i,2)-circles(i,4)
							If circles(i,1)+circles(i,4)>extentsx2 Then extentsx2=circles(i,1)+circles(i,4)
							If circles(i,2)+circles(i,4)>extentsy2 Then extentsy2=circles(i,2)+circles(i,4)
						End If
					Case 2 To 4
						If circles(i,9) = 2 Then
							boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6)*180/pi,circles(i,7)*180/pi,circles(i,4),circles(i,11))
						Else
							boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
						EndIf
						If blockextentsset=FALSE Then
							blockextentsset=TRUE
							extentsx1=boxarcx1
							extentsx2=boxarcx2
							extentsy1=boxarcy1
							extentsy2=boxarcy2
						Else
							If boxarcx1<extentsx1 Then extentsx1=boxarcx1
							If boxarcy1<extentsy1 Then extentsy1=boxarcy1
							If boxarcx2>extentsx2 Then extentsx2=boxarcx2
							If boxarcy2>extentsy2 Then extentsy2=boxarcy2
						End If
				End Select
			End If
		Next

		blockoffsets(blockc,1)=0
		blockoffsets(blockc,2)=0
		blockoffsets(blockc,3)=0
		
		modifyx1=extentsx1
		modifyy1=extentsy1
		modifyx2=0
		modifyy2=0
		movedxfblock(blockc)
		blockstatus(blockc)=-1
		For i = 1 To linec
			If lines(i,9)=blockc Then lines(i,8)=-1
		Next
		For i = 1 To circlec
			If circles(i,12)=blockc Then circles(i,10)=-1
		Next
		
		blockc=blockc+1
		i=blockc-1
		tempint=linec
		For j=1 To tempint
			If lines(j,9)=i Then
				memmanageline
				For k=1 To 9
					lines(linec,k)=lines(j,k)
				Next
				If dimensioning=TRUE Then lines(linec,8)=1 Else lines(linec,8)=0
				'lines(linec,8)=0
				lines(linec,9)=blockc
			End If
		Next
		tempint=circlec
		For j=1 To tempint
			If circles(j,12)=i Then
				memmanagecircle
				For k=1 To 12
					circles(circlec,k)=circles(j,k)
				Next
				If dimensioning=TRUE Then circles(circlec,10)=1 Else circles(circlec,10)=0
				'circles(circlec,10)=0
				circles(circlec,12)=blockc
			EndIf
		Next

		modifyx1=0
		modifyy1=0
		modifyx2=tempxoffset
		modifyy2=tempyoffset
		movedxfblock(blockc)
		blocknames(blockc)=blocknames(blockc-1)
		blockoffsets(blockc,1)=tempxoffset
		blockoffsets(blockc,2)=tempyoffset
		blockoffsets(blockc,3)=0
		blockoffsets(blockc,4)=1
		blockoffsets(blockc,5)=1
		blockoffsets(blockc,6)=1
		blockoffsets(blockc,7)=0
		inview()
		redraw
	End if
End Sub
Sub scaleit(objecttype As String,objectarrayvalue As Integer, scalexamount As Double, scaleyamount As Double, scalezamount As Double, scalefromx As Double, scalefromy As Double)
	Select Case objecttype
		Case "LINE"
			lines(objectarrayvalue,1)=scalefromx+(lines(objectarrayvalue,1)-scalefromx)*scalexamount
			lines(objectarrayvalue,4)=scalefromx+(lines(objectarrayvalue,4)-scalefromx)*scalexamount
			lines(objectarrayvalue,2)=scalefromy+(lines(objectarrayvalue,2)-scalefromy)*scaleyamount
			lines(objectarrayvalue,5)=scalefromy+(lines(objectarrayvalue,5)-scalefromy)*scaleyamount
		Case "CIRCLE"
			Select Case circles(objectarrayvalue,9)
				Case 1'circles
					circles(objectarrayvalue,1)=scalefromx+(circles(objectarrayvalue,1)-scalefromx)*scalexamount
					circles(objectarrayvalue,2)=scalefromy+(circles(objectarrayvalue,2)-scalefromy)*scaleyamount
					If scalexamount<>scaleyamount Then
						circles(objectarrayvalue,8)=circles(objectarrayvalue,4)
						circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scalexamount
						circles(objectarrayvalue,8)=circles(objectarrayvalue,8)*scaleyamount
						circles(objectarrayvalue,6)=0
						circles(objectarrayvalue,7)=360
						circles(objectarrayvalue,9)=3
					Else	
						circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scalexamount
					End If
				Case 2'arcs
					circles(objectarrayvalue,1)=scalefromx+(circles(objectarrayvalue,1)-scalefromx)*scalexamount
					circles(objectarrayvalue,2)=scalefromy+(circles(objectarrayvalue,2)-scalefromy)*scaleyamount
					If scalexamount<>scaleyamount Then
						circles(objectarrayvalue,8)=circles(objectarrayvalue,4)
						circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scalexamount
						circles(objectarrayvalue,8)=circles(objectarrayvalue,8)*scaleyamount
						circles(objectarrayvalue,6)=circles(objectarrayvalue,6)*180/pi
						circles(objectarrayvalue,7)=circles(objectarrayvalue,7)*180/pi
						circles(objectarrayvalue,9)=4
					Else	
						circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scalexamount
					End If
				Case 3,4'ellipses and elliptical arcs
					If scalexamount=scaleyamount Then
						circles(objectarrayvalue,1)=scalefromx+(circles(objectarrayvalue,1)-scalefromx)*scalexamount
						circles(objectarrayvalue,2)=scalefromy+(circles(objectarrayvalue,2)-scalefromy)*scaleyamount
						circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scalexamount
						circles(objectarrayvalue,8)=circles(objectarrayvalue,8)*scaleyamount
					Else
						Select Case circles(objectarrayvalue,11)
							Case 0,180,360
								circles(objectarrayvalue,1)=scalefromx+(circles(objectarrayvalue,1)-scalefromx)*scalexamount
								circles(objectarrayvalue,2)=scalefromy+(circles(objectarrayvalue,2)-scalefromy)*scaleyamount
								circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scalexamount
								circles(objectarrayvalue,8)=circles(objectarrayvalue,8)*scaleyamount
							Case 90,270
								circles(objectarrayvalue,1)=scalefromx+(circles(objectarrayvalue,1)-scalefromx)*scalexamount
								circles(objectarrayvalue,2)=scalefromy+(circles(objectarrayvalue,2)-scalefromy)*scaleyamount
								circles(objectarrayvalue,4)=circles(objectarrayvalue,4)*scaleyamount
								circles(objectarrayvalue,8)=circles(objectarrayvalue,8)*scalexamount
							Case Else
								selcircle=objectarrayvalue
								monkeysmatter2(scalefromx,scalefromy,scalexamount,scaleyamount)
						End Select
					End If
			End Select
	End Select
End Sub
Sub inviewfilter()
	Dim As Integer i,j,iv
	Dim As BOOLEAN filter
	inviewfilterlinesc=0
	inviewfiltercirclesc=0
	'filtertype(1)="Y"'use filters
	'filtertype(2)="N"'filter all blocks
	'filtertype(3)="Y"'filter text blocks
	''filtertyp(4-n)=(y/n) filter by block name
	If filtertype(1,1)="N" Then'don't use filters
		For i = 1 To inviewlinesc
			iv=inviewlines(i)
			inviewfilterlinesc=inviewfilterlinesc+1
			inviewfilterlines(inviewfilterlinesc)=iv
		Next
		For i = 1 To inviewcirclesc
			iv=inviewcircles(i)
			inviewfiltercirclesc=inviewfiltercirclesc+1
			inviewfiltercircles(inviewfiltercirclesc)=iv
		Next
	Else'use filters
		If filtertype(2,1)="N" Then'don't filter all blocks
			If filtertype(3,1)="N" Then'don't filter text blocks
				'here is where to add an for next for all block names
				'in order to filter specific blocks by name
				For i = 1 To inviewlinesc
					iv=inviewlines(i)
					inviewfilterlinesc=inviewfilterlinesc+1
					inviewfilterlines(inviewfilterlinesc)=iv
				Next
				For i = 1 To inviewcirclesc
					iv=inviewcircles(i)
					inviewfiltercirclesc=inviewfiltercirclesc+1
					inviewfiltercircles(inviewfiltercirclesc)=iv
				Next
			Else'filter text blocks
				For i = 1 To inviewlinesc
					iv=inviewlines(i)
					If Mid(blocknames(lines(iv,9)),1,6)<>"fbctxt" Then'entity is not part of a text block
						inviewfilterlinesc=inviewfilterlinesc+1
						inviewfilterlines(inviewfilterlinesc)=iv
					End If
				Next
				For i = 1 To inviewcirclesc
					iv=inviewcircles(i)
					If Mid(blocknames(circles(iv,12)),1,6)<>"fbctxt" Then'entity is not part of a text block
						inviewfiltercirclesc=inviewfiltercirclesc+1
						inviewfiltercircles(inviewfiltercirclesc)=iv
					End If
				Next
			End If
		Else'filter all blocks no mater what type
			For i = 1 To inviewlinesc
				iv=inviewlines(i)
				If lines(iv,9)=0 Then'meaning not part of a block
					inviewfilterlinesc=inviewfilterlinesc+1
					inviewfilterlines(inviewfilterlinesc)=iv
				End If
			Next
			For i = 1 To inviewcirclesc
				iv=inviewcircles(i)
				If circles(iv,12)=0 Then'meaning not part of a block
					inviewfiltercirclesc=inviewfiltercirclesc+1
					inviewfiltercircles(inviewfiltercirclesc)=iv
				End If
			Next
		End If
	End If
	pae2gsbadj_config(0,inviewfilterlinesc)
	pae3gsbadj_config(0,inviewfiltercirclesc)
	If inviewfilterlinesc=0 Then
		gtk_entry_set_text(GTK_ENTRY(pae2ge1),"")
		gtk_entry_set_text(GTK_ENTRY(pae2ge2),"")
		gtk_entry_set_text(GTK_ENTRY(pae2ge3),"")
		gtk_entry_set_text(GTK_ENTRY(pae2ge4),"")
		gtk_entry_set_text(GTK_ENTRY(pae2ge5),"")
		gtk_entry_set_text(GTK_ENTRY(pae2ge6),"")
		gtk_expander_set_label (paexpander2,"Line Properties")
		gtk_widget_grab_focus(ebox)
	Else
		pae2gsbadj_value_changed(NULL, NULL)
	EndIf
	If inviewfiltercirclesc=0 Then
		gtk_entry_set_text(GTK_ENTRY(pae3ge1),"")
		gtk_entry_set_text(GTK_ENTRY(pae3ge2),"")
		gtk_entry_set_text(GTK_ENTRY(pae3ge3),"")
		gtk_entry_set_text(GTK_ENTRY(pae3ge4),"")
		gtk_entry_set_text(GTK_ENTRY(pae3ge5),"")
		gtk_entry_set_text(GTK_ENTRY(pae3ge6),"")
		gtk_entry_set_text(GTK_ENTRY(pae3ge7),"")
		gtk_expander_set_label (paexpander3,"Circle Properties")
		gtk_widget_grab_focus(ebox)
	Else
		pae3gsbadj_value_changed(NULL, NULL)
	EndIf
End Sub
Sub inview()
	Dim As Integer i,iv
	inviewlinesc=0
	inviewcirclesc=0
	For i = preloadedlinec+1 To linec
		If lines(i,8)>-1 Then
			iv=0'false
			Select case layerstate(lines(i,7))
				Case 1,2
					Select case lines(i,1)
						Case wx1 To wx2
							Select case lines(i,2)
								Case wy1 To wy2
									iv=1
								Case Else
									Select case lines(i,4)
										Case wx1 To wx2
											Select case lines(i,5)
												Case wy1 To wy2
													iv=1
											End Select
									End Select
							End Select
						Case Else
							Select case lines(i,4)
								Case wx1 To wx2
									Select case lines(i,5)
										Case wy1 To wy2
											iv=1
									End Select
							End Select
					End Select
			End Select
			If iv=1 Then'ytrue
				'addit to inveiwlines array
				inviewlinesc=inviewlinesc+1
				inviewlines(inviewlinesc)=i
			EndIf
		End if
	Next
	For i = preloadedcirclec+1 To circlec
		If circles(i,10)>-1 then
			iv=0
			Select case layerstate(circles(i,5))
				Case 1,2
					Select Case circles(i,9)
						Case 1
							Select case circles(i,1)
								Case wx1 To wx2
									Select case circles(i,2)
										Case wy1 To wy2
											iv=1
									End Select
							End Select
						Case Else
							If circles(i,9) = 2 Then
								boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6)*180/pi,circles(i,7)*180/pi,circles(i,4),circles(i,11))
							Else
								boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
							EndIf
							Select case boxarcx1'top left
								Case wx1 To wx2
									Select case boxarcy2
										Case wy1 To wy2
											iv=1
									End Select
							End Select
							Select case boxarcx2'top right
								Case wx1 To wx2
									Select case boxarcy2
										Case wy1 To wy2
											iv=1
									End Select
							End Select
							Select case boxarcx1'top left
								Case wx1 To wx2
									Select case boxarcy1
										Case wy1 To wy2
											iv=1
									End Select
							End Select
							Select case boxarcx2'top right
								Case wx1 To wx2
									Select case boxarcy1
										Case wy1 To wy2
											iv=1
									End Select
							End Select
					End Select

					
					'Select case circles(i,1)
					'	Case wx1 To wx2
					'		Select case circles(i,2)
					'			Case wy1 To wy2
					'				iv=1
					'			Case Else
					'				Select case circles(i,2)+circles(i,4)
					'					Case wy1 To wy2
					'						iv=1
					'					Case Else
					'						Select case circles(i,2)-circles(i,4)
					'							Case wy1 To wy2
					'								iv=1
					'						End Select
					'				End Select
					'		End Select
					'	Case Else
					'		Select case circles(i,1)+circles(i,4)
					'			Case wx1 To wx2
					'				Select case circles(i,2)
					'					Case wy1 To wy2
					'						iv=1
					'				End Select
					'			Case Else
					'				Select case circles(i,1)-circles(i,4)
					'					Case wx1 To wx2
					'						Select case circles(i,2)
					'							Case wy1 To wy2
					'								iv=1
					'						End Select
					'				End Select
					'		End Select
					'End Select
			End Select
			If iv=1 Then
				'addit to inveiwcircles array
				inviewcirclesc=inviewcirclesc+1
				inviewcircles(inviewcirclesc)=i
			EndIf
		End if
	Next
	inviewfilter
End Sub
Sub calcellipsepoint(cepi As Integer)
	Dim As Double cx1,cy1,cr1,cr2, cx2,cy2,cr3,cr4,erot1,erot2
	Dim As Integer i
	Dim As Double c1eplotx1,c1eploty1,c1eplotx2,c1eploty2
	Dim As Double c2eplotx1,c2eploty1,c2eplotx2,c2eploty2
	Dim As Double anglerange

	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	Dim As Double fxmt,fymt,fxmtp,fymtp
	Dim As Double fxmtp1,fymtp1,fxmtp2,fymtp2

	skipecheck=FALSE

	cx1=circles(cepi,1)'cx1,cy1 are the values of the center of the ellipse
	cy1=circles(cepi,2)
	cr1=circles(cepi,4)'cr1,cr2 are the major & minor radius values of the ellipse
	cr2=circles(cepi,8)
	erot1=circles(cepi,11)

'	window (wx1,wy1)-(wx2,wy2)

	fxmtp=cx1
	fymtp=cy1
	
'	fxmt=dpmx
'	fymt=dpmy
	fxmt=mousex
	fymt=mousey
'	fxmt=scrnmx
'	fymt=scrnmy

	anglerange=10
	Dim As Double arx1,ary1,arx2,ary2

'		arx1=cx1
'		ary1=cy1
		arx2=fxmt
		ary2=fymt
		x1p=arx2-cx1
		y1p=(ary2-cy1)
		arx2=x1p*Cos(-erot1*d2r) - y1p*Sin(-erot1*d2r)+cx1
		ary2=y1p*Cos(-erot1*d2r) + x1p*Sin(-erot1*d2r)+cy1
		ylength=(ary2-cy1)*(cr1/cr2)
		xlength=arx2-cx1
		fixangle
		arcstart=angle -anglerange
		If arcstart<0 Then arcstart=360+arcstart
		arcend=angle +anglerange
		If arcend>360 Then arcend=arcend-360
		
		If circles(cepi,6)<circles(cepi,7) Then
			Select Case arcstart
				Case circles(cepi,6) To circles(cepi,7)
					epastart=arcstart
					Select Case arcend
						Case circles(cepi,6) To circles(cepi,7)
							epaend=arcend
						Case Else
							epaend=circles(cepi,7)
					End Select
				Case Else
					If arcend > arcstart Then
						Select Case arcend
							Case circles(cepi,6) To circles(cepi,7)
								epastart=circles(cepi,6)
								epaend=arcend
							Case Else
								'the arc range to check is not in the ellipse
								'skip checking this one
								skipecheck=TRUE
						End Select
					Else
						Select Case angle
							Case circles(cepi,6) To circles(cepi,7)
								epastart=circles(cepi,6)
								epaend=circles(cepi,7)
							Case Else
								'the arc range to check is not in the ellipse
								'skip checking this one
								skipecheck=TRUE
						End Select
					End if
			End Select
		Else'''''''''''''''''
			Select Case arcstart
				Case circles(cepi,6) To 360, 0 To circles(cepi,7)
					epastart=arcstart
					Select Case arcend
						Case circles(cepi,6) To 360, 0 To circles(cepi,7)
							epaend=arcend
						Case Else
							epaend=circles(cepi,7)
					End Select
				Case Else
					If arcend > arcstart Then
						Select Case arcend
							Case circles(cepi,6) To 360, 0 To circles(cepi,7)
								epastart=circles(cepi,6)
								epaend=arcend
							Case Else
								'the arc range to check is not in the ellipse
								'skip checking this one
								skipecheck=TRUE
						End Select
					Else
						Select Case angle
							Case circles(cepi,6) To 360, 0 To circles(cepi,7)
								epastart=circles(cepi,6)
								epaend=circles(cepi,7)
							Case Else
								'the arc range to check is not in the ellipse
								'skip checking this one
								skipecheck=TRUE
						End Select
					End if
			End Select
		EndIf
End Sub
sub saveblockdxf()
	'if there are no entities selected to be in the new block
	'then decrement blockc
	'bec is block entity count
	'write the minimal dxf for the entities in this block
	'by calling the dxf routines
End Sub
sub importblock()
	'needs to import all entities and INSERTS of blocks (if any)
	'into a new block
	Dim As Integer i
	Dim As String itemstring
	Dim As Integer templinec,tempcirclec
	templinec=linec
	tempcirclec=circlec
	'screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	tempstring=blockname+".dxf"
	'need to see if block has already been loaded
	'befor i load it twice here
	open tempstring for input as #1
	'need to make open and save as dlg and then deal with err=0 stuff
	'If Err<>0 Then Exit Sub
	
	blockc=blockc+1
	ReDim Preserve blocknames(blockc)
	ReDim Preserve blockoffsets(blockc,7)
	ReDim Preserve blockstatus(blockc)
	blocknames(blockc)=blockname
	blockstatus(blockc)=-1
	itemstring=""
	
	do while not eof(1)
		'load arrays

		line input #1, tempstring

		Select Case tempstring
			Case "  0"
				itemstring=""
			Case "LINE"
				memmanageline
				itemstring="LINE"
				lines(linec,7)=15
				lines(linec,8)=-1
				lines(linec,9)=blockc
			Case "CIRCLE"
				memmanagecircle
				circles(circlec,9)=1
				itemstring="CIRCLE"
				circles(circlec,5)=15
				circles(circlec,10)=-1
				circles(circlec,12)=blockc
			Case "ARC"
				memmanagecircle
				circles(circlec,9)=2
				itemstring="ARC"
				circles(circlec,5)=15
				circles(circlec,10)=-1
				circles(circlec,12)=blockc
			Case "ELLIPSE"
				memmanagecircle
				itemstring="ELLIPSE"
				circles(circlec,5)=15
				circles(circlec,10)=-1
				circles(circlec,12)=blockc
		End Select
		Select Case itemstring
			Case "LINE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							lines(linec,7)=val(mid$(tempstring,8))
						Else
							lines(linec,7)=Val(tempstring) Mod 15
							If lines(linec,7)=0 Then lines(linec,7)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						lines(linec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						lines(linec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						lines(linec,3)=val(tempstring)
					Case " 11"
						line input #1, tempstring
						lines(linec,4)=val(tempstring)
					Case " 21"
						line input #1, tempstring
						lines(linec,5)=val(tempstring)
					Case " 31"
						line input #1, tempstring
						lines(linec,6)=val(tempstring)
				End Select

			Case "CIRCLE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 40"
						line input #1, tempstring
						circles(circlec,4)=val(tempstring)
				End Select
			Case "ARC"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 40"
						line input #1, tempstring
						circles(circlec,4)=val(tempstring)
					Case " 50"
						line input #1, tempstring
						circles(circlec,6)=val(tempstring)*d2r
					Case " 51"
						line input #1, tempstring
						circles(circlec,7)=val(tempstring)*d2r
				End Select
			Case "ELLIPSE"
				Select Case tempstring
					Case "  8"
						line input #1, tempstring
						If Mid$(tempstring,1,7)="MYLAYER" Then
							circles(circlec,5)=val(mid$(tempstring,8))
						Else
							circles(circlec,5)=Val(tempstring) Mod 15
							If circles(circlec,5)=0 Then circles(circlec,5)=15
						EndIf
					Case " 10"
						line input #1, tempstring
						circles(circlec,1)=val(tempstring)
					Case " 20"
						line input #1, tempstring
						circles(circlec,2)=val(tempstring)
					Case " 30"
						line input #1, tempstring
						circles(circlec,3)=val(tempstring)
					Case " 11"
						line input #1, tempstring
						x2=val(tempstring)
					Case " 21"
						line input #1, tempstring
						x1=circles(circlec,1)
						y1=circles(circlec,2)
						y2=val(tempstring)
						x2=x1+x2
						y2=y1+y2
						circles(circlec,4)=(sqr((x1-x2)^2 + (y1-y2)^2))
					Case " 31"
						line input #1, tempstring
					Case " 40"
						line input #1, tempstring
						circles(circlec,8)=circles(circlec,4)*val(tempstring)
						trackangle
						circles(circlec,11)=angle
					Case " 41"
						line input #1, tempstring
						circles(circlec,6)=val(tempstring)*180/pi
					Case " 42"
						line input #1, tempstring
						circles(circlec,7)=val(tempstring)*180/pi
						If circles(circlec,6)=0 And circles(circlec,7)=360 Then
							circles(circlec,9)=3
						Else
							circles(circlec,9)=4
						EndIf
				End Select
		End Select
	loop
	close #1


	'what if no entities were loaded???
	'also the only reason i figure out the extents of the block
	'is to estimate a good insertion point for the block.
	'normally blocks have insertion xyz point in the dxf file
	'using entity type INSERT insertion point as the insertion point
	'but in the event of importing an entiry drawing as a block
	'then an estimated insertion point is needed
	'and in the case, the lower left corner of a bounding box
	'is appropriate.
	'so this sub routine is really just for importing complete drawings
	'into a block. i still need to code internal block entity importing
	If edit_master_block=TRUE Then
		'don't move it
	Else
		If linec>templinec Then
			extentsx1=lines(templinec+1,1)
			extentsy1=lines(templinec+1,2)
		Else
			extentsx1=circles(tempcirclec+1,1)-circles(tempcirclec+1,4)
			extentsy1=circles(tempcirclec+1,2)-circles(tempcirclec+1,4)
		EndIf
		For i = templinec+1 To linec
			If lines(i,1)<extentsx1 Then extentsx1=lines(i,1)'-100
			If lines(i,4)<extentsx1 Then extentsx1=lines(i,4)'-100
			If lines(i,2)<extentsy1 Then extentsy1=lines(i,2)'-100
			If lines(i,5)<extentsy1 Then extentsy1=lines(i,5)'-100
		Next
		For i = tempcirclec+1 To circlec
			If circles(i,1)-circles(i,4)<extentsx1 Then extentsx1=circles(i,1)-circles(i,4)
			If circles(i,1)-circles(i,8)<extentsx1 Then extentsx1=circles(i,1)-circles(i,8)
			If circles(i,2)-circles(i,4)<extentsy1 Then extentsy1=circles(i,2)-circles(i,4)
			If circles(i,2)-circles(i,8)<extentsy1 Then extentsy1=circles(i,2)-circles(i,8)

						'If circles(i,9) = 2 Then
						'	boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6)*180/pi,circles(i,7)*180/pi,circles(i,4),circles(i,11))
						'Else
						'	boxarc(i,circles(i,1),circles(i,2),circles(i,3),circles(i,4),31,circles(i,6),circles(i,7),circles(i,8),circles(i,11))
						'EndIf
						'If blockextentsset=FALSE Then
						'	blockextentsset=TRUE
						'	extentsx1=boxarcx1
						'	extentsx2=boxarcx2
						'	extentsy1=boxarcy1
						'	extentsy2=boxarcy2
						'Else
						'	If boxarcx1<extentsx1 Then extentsx1=boxarcx1
						'	If boxarcy1<extentsy1 Then extentsy1=boxarcy1
						'	If boxarcx2>extentsx2 Then extentsx2=boxarcx2
						'	If boxarcy2>extentsy2 Then extentsy2=boxarcy2
						'End If




		Next
		modifyx1=extentsx1
		modifyy1=extentsy1
		modifyx2=0
		modifyy2=0
		movedxfblock(blockc)
	EndIf
	blockoffsets(blockc,1)=0
	blockoffsets(blockc,2)=0
	blockoffsets(blockc,3)=0
	blockoffsets(blockc,4)=1
	blockoffsets(blockc,5)=1
	blockoffsets(blockc,6)=1
	blockoffsets(blockc,7)=0

	blockc=blockc+1
	ReDim Preserve blocknames(blockc)
	ReDim Preserve blockoffsets(blockc,7)
	ReDim Preserve blockstatus(blockc)
	blocknames(blockc)=blockname
	blockoffsets(blockc,3)=0
	blockoffsets(blockc,4)=1
	blockoffsets(blockc,5)=1
	blockoffsets(blockc,6)=1
	blockoffsets(blockc,7)=0
	Dim As Integer eoblinec,eobcirclec,j
	eoblinec=linec
	eobcirclec=circlec
	for i = templinec+1 to eoblinec
		memmanageline
		For j=1 To 9
			lines(linec,j)=lines(i,j)
		Next
		lines(linec,8)=2
		lines(linec,9)=blockc
	Next
	For i = tempcirclec+1 To eobcirclec
		memmanagecircle
		For j=1 To 12
			circles(circlec,j)=circles(i,j)
		Next
		circles(circlec,10)=2
		circles(circlec,12)=blockc
	Next
	'this will be faster then movebock routine
	
	If buttonson(113)=TRUE Then
		blockoffsets(blockc,1)=insertionx
		blockoffsets(blockc,2)=insertiony
		modifyx1=0
		modifyy1=0
		modifyx2=insertionx
		modifyy2=insertiony
		for i = eoblinec+1 to linec
			lines(i,1)=lines(i,1)+(modifyx2-modifyx1)
			lines(i,2)=lines(i,2)+(modifyy2-modifyy1)
			lines(i,3)=lines(i,3)+(modifyz2-modifyz1)
			lines(i,4)=lines(i,4)+(modifyx2-modifyx1)
			lines(i,5)=lines(i,5)+(modifyy2-modifyy1)
			lines(i,6)=lines(i,6)+(modifyz2-modifyz1)
		Next
		For i = eobcirclec+1 To circlec
			circles(i,1)=circles(i,1)+(modifyx2-modifyx1)
			circles(i,2)=circles(i,2)+(modifyy2-modifyy1)
		Next
	Else
		blockoffsets(blockc,1)=0
		blockoffsets(blockc,2)=0
		modifyx1=0
		modifyy1=0
	End If
End Sub
Sub copybaseblock(cbbi As Integer)
	Dim As Integer i,j,k
	i=cbbi
	blockc=blockc+1
	ReDim Preserve blocknames(blockc)
	ReDim Preserve blockoffsets(blockc,7)
	ReDim Preserve blockstatus(blockc)
	blocknames(blockc)=blocknames(i)
	blockoffsets(blockc,1)=blockoffsets(i,1)
	blockoffsets(blockc,2)=blockoffsets(i,2)
	blockoffsets(blockc,3)=blockoffsets(i,3)
	blockoffsets(blockc,4)=blockoffsets(i,4)
	blockoffsets(blockc,5)=blockoffsets(i,5)
	blockoffsets(blockc,6)=blockoffsets(i,6)
	blockoffsets(blockc,7)=blockoffsets(i,7)
	
	Dim As Integer eoblinec,eobcirclec
	eoblinec=linec
	eobcirclec=circlec	
	tempint=linec
	For j=1 To tempint
		If lines(j,9)=i Then
			memmanageline
			For k=1 To 9
				lines(linec,k)=lines(j,k)
			Next
			lines(linec,8)=1
			lines(linec,9)=blockc
		End If
	Next
	tempint=circlec
	For j=1 To tempint
		If circles(j,12)=i Then
			memmanagecircle
			For k=1 To 12
				circles(circlec,k)=circles(j,k)
			Next
			circles(circlec,10)=1
			circles(circlec,12)=blockc
		EndIf
	Next
	
	'screenset 0,0:view (drawareax1,drawareay1)-(drawareax2,drawareay2):window (wx1,wy1)-(wx2,wy2)
	
	If buttonson(113)=TRUE Then
		blockoffsets(blockc,1)=insertionx
		blockoffsets(blockc,2)=insertiony
		modifyx1=0
		modifyy1=0
		modifyx2=insertionx
		modifyy2=insertiony
		for i = eoblinec+1 to linec
			lines(i,1)=lines(i,1)+(modifyx2-modifyx1)
			lines(i,2)=lines(i,2)+(modifyy2-modifyy1)
			lines(i,3)=lines(i,3)+(modifyz2-modifyz1)
			lines(i,4)=lines(i,4)+(modifyx2-modifyx1)
			lines(i,5)=lines(i,5)+(modifyy2-modifyy1)
			lines(i,6)=lines(i,6)+(modifyz2-modifyz1)
		Next
		For i = eobcirclec+1 To circlec
			circles(i,1)=circles(i,1)+(modifyx2-modifyx1)
			circles(i,2)=circles(i,2)+(modifyy2-modifyy1)
		Next
	Else
		modifyx1=0
		modifyy1=0
	End If
End Sub
Sub adjustprecision()
	Dim As Integer i
	dimvalue=Abs(dimvalue)
	'add rounding to this routine
	Select Case dimprecision
		Case 0
			'dimvalue=cint(dimvalue)
			tempstring=Str(dimvalue)
		Case Is > 0
			'dimvalue=dimvalue*10^dimprecision
			'dimvalue=cint(dimvalue)
			If dimvalue=Int(dimvalue) Then
				tempstring=Str(dimvalue)
			Else
				If InStr(Str(dimvalue-Int(dimvalue)),"e")=0 then
					tempstring=Str(Int(dimvalue)) + "." + Mid$(Str(dimvalue-Int(dimvalue)),3,dimprecision)
				Else
					tempstring=Str(Int(dimvalue)) + ".0"
					For i = 1 To dimprecision
						If Mid(Str(dimvalue-Int(dimvalue)) ,i,1)<>"." Then tempstring=tempstring+Mid(Str(dimvalue-Int(dimvalue)) ,i,1)
					Next
					'Draw String (mousex,mousey),tempstring
				End If
				dimvalue=Val(tempstring)
				'dimvalue=dimvalue/10^dimprecision
			End If
	End Select
	dimvaluestring=tempstring
End Sub
Sub calctangentpoint()
	'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
	Select Case circles(selcircle,9)
		Case 1,2
			Dim As Double adjacent,opposite,hypotenuse,theta
			Dim As Double tempx1,tempy1,tp1x,tp1y,tp2x,tp2y,tempselangle
			tempx1=x1
			tempy1=y1
			hypotenuse=calcd(x1,y1,z1,circles(selcircle,1),circles(selcircle,2),0)
			adjacent=circles(selcircle,4)
			'a^2+b^2=c^2
			opposite=Sqr(hypotenuse^2-adjacent^2)
			'sin(theta)=opposite/hypotenuse
			'cos(theta)=adjacent/hypotenuse
			'the inverse of sine is arcsine or asin
			'the inverse of cosine is arccosine or acos
			'asin return the angle in radians
			'theta=ASin(opposite/hypotenuse)
			theta=ACos(adjacent/hypotenuse)
			theta=theta*(180/PI)
			'calc angle from circl center to x1,y1
			'add and subtract this theta from this angle
			'these angle from the center of the circle at length of radius
			'are the two tangent points
			memmanageline
			lines(linec,1)=circles(selcircle,1)
			lines(linec,2)=circles(selcircle,2)
			lines(linec,4)=x1
			lines(linec,5)=y1
			selline=linec
			calcselangle
			tempselangle=selangle
			selangle=tempselangle+theta
			sellength=circles(selcircle,4)
			x1=circles(selcircle,1)
			y1=circles(selcircle,2)
			altosellength'returns fx,fy
			tp1x=fx
			tp1y=fy
			
			selangle=tempselangle-theta
			sellength=circles(selcircle,4)
			x1=circles(selcircle,1)
			y1=circles(selcircle,2)
			altosellength'returns fx,fy
			tp2x=fx
			tp2y=fy
			linec=linec-1
			If calcd(tp1x,tp1y,0,CDbl(mousex),CDbl(mousey),0)<calcd(tp2x,tp2y,0,CDbl(mousex),CDbl(mousey),0) Then
				fxm=tp1x
				fym=tp1y
			Else
				fxm=tp2x
				fym=tp2y
			End If
			x1=tempx1
			y1=tempy1
	
			
		Case 3,4
			'tx1 ty1 is the x,y of the first foci tx2 is the second foci
			Dim As Double tx1,ty1,tx2,ty2,er1,er2,ex1,ey1,eangle,foci
			er1=circles(selcircle,4)
			er2=circles(selcircle,8)
			ex1=circles(selcircle,1)
			ey1=circles(selcircle,2)
			eangle=circles(selcircle,11)
			If er1>er2 Then
				foci=Sqr(er1^2-er2^2)
				tx1=ex1+cos(eangle*d2r)*foci
				ty1=ey1+sin(eangle*d2r)*foci
				'Circle(tx1,ty1),2,10
				tx2=ex1-cos(eangle*d2r)*foci
				ty2=ey1-sin(eangle*d2r)*foci
				'Circle(tx2,ty2),2,10
			Else
				foci=Sqr(er2^2-er1^2)
				tx1=ex1+cos((eangle+90)*d2r)*foci
				ty1=ey1+sin((eangle+90)*d2r)*foci
				'Circle(tx1,ty1),2,10
				tx2=ex1-cos((eangle+90)*d2r)*foci
				ty2=ey1-sin((eangle+90)*d2r)*foci
				'Circle(tx2,ty2),2,10
			End If
			'figure out which foci x1,y1 is nearest
			if sqr((x1-tx1)^2 + (y1-ty1)^2) < sqr((x1-tx2)^2 + (y1-ty2)^2) Then
				swap tx1,tx2
				swap ty1,ty2
			End If
			'closer to tx2,ty2
			'create a circle centered at the first foci
			'with radius of that of the major axis
			'Circle (tx1,ty1),er1*2
			'Circle (x1,y1),calcd(x1,y1,0,tx2,ty2,0)
			'intersection of two circles
			Dim As Double cx1,cy1,cr1,cx2,cy2,cr2
			Dim As Double A1,B1,C1,det,dx1,dy1,fxm1,fxm2,fym1,fym2
			cx1=tx1
			cy1=ty1
			cr1=er1*2
			cx2=x1
			cy2=y1
			cr2=calcd(x1,y1,0,tx2,ty2,0)
			dx1 = cx2 - cx1
			dy1 = cy2 - cy1
			A1 = (cr1^2 - cr2^2 + dx1^2 + dy1^2) / (2.0 * sqr(dx1^2 + dy1^2))
			B1 = sqr(abs(cr1^2 - A1^2))
			fxm1 = cx1 + (dx1 * A1/sqr(dx1^2 + dy1^2)) + -dy1 * (B1/sqr(dx1^2 + dy1^2))
			fxm2 = cx1 + (dx1 * A1/sqr(dx1^2 + dy1^2)) - (-dy1 * (B1/sqr(dx1^2 + dy1^2)))
			fym1 = cy1 + (dy1 * A1/sqr(dx1^2 + dy1^2)) + dx1 * (B1/sqr(dx1^2 + dy1^2))
			fym2 = cy1 + (dy1 * A1/sqr(dx1^2 + dy1^2)) - dx1 * (B1/sqr(dx1^2 + dy1^2))
			'Line(tx1,ty1)-(fxm1,fym1)
			'Line(tx1,ty1)-(fxm2,fym2)
			'calclei(ax1,ay1,ax2,ay2,cx1,cy1,cr1,cr2,erot)
			Dim As Double tp1x,tp1y,tp2x,tp2y
			Dim As Double tx1t,ty1t,tx2t,ty2t,fxm1t,fym1t,fxm2t,fym2t

			tx1t=tx1
			ty1t=ty1
			tx2t=tx2
			ty2t=ty2
			fxm1t=fxm1
			fym1t=fym1
			fxm2t=fxm2
			fym2t=fym2
			calclei(tx1,ty1,fxm2,fym2,ex1,ey1,er1,er2,eangle)
			'Circle(fxm,fym),10,10
			tp2x=fxm
			tp2y=fym

			tx1=tx1t
			ty1=ty1t
			tx2=tx2t
			ty2=ty2t
			fxm1=fxm1t
			fym1=fym1t
			fxm2=fxm2t
			fym2=fym2t

			calclei(tx1,ty1,fxm1,fym1,ex1,ey1,er1,er2,eangle)
			'Circle(fxm,fym),10,11
			tp1x=fxm
			tp1y=fym

			'check to see which tangent point the mouse is nearest to
			if sqr((mousex-tp1x)^2 + (mousey-tp1y)^2) < sqr((mousex-tp2x)^2 + (mousey-tp2y)^2) Then
				fxm=tp1x
				fym=tp1y
			Else
				fxm=tp2x
				fym=tp2y
			End If
	End Select
End Sub
Sub calclei(ax1 As Double,ay1 As Double,ax2 As Double,ay2 As Double,cx1 As Double,cy1 As Double,cr1 As Double,cr2 As Double,erot As double)
	Dim As Double A1,B1,C1,det,dx1,dy1,fxm1,fxm2,fym1,fym2,erotx,eroty
'calclei(ax1,ay1,ax2,ay2,cx1,cy1,cr1,cr2,erot)

	If erot<>0 Then 'if the ellipse is rotated then the line needs to be rotated
		x1p=ax1-cx1
		y1p=ay1-cy1
		x2p=ax2-cx1
		y2p=ay2-cy1
		ax1=x1p*cos(-erot*d2r) - y1p*sin(-erot*d2r)+cx1
		ay1=y1p*cos(-erot*d2r) + x1p*sin(-erot*d2r)+cy1
		ax2=x2p*cos(-erot*d2r) - y2p*sin(-erot*d2r)+cx1
		ay2=y2p*cos(-erot*d2r) + x2p*sin(-erot*d2r)+cy1
	EndIf
	dx1 = ax2 - ax1'dx1 is the X distance between the end points of a line
	dy1 = ay2 - ay1'dy1 is the Y distance between the end points of a line
	A1 = (1 / (dy1/dx1))^2 + (cr1 / cr2)^2
	B1 = (2 / (dy1/dx1)) * ((ax1-cx1) - (ay1-cy1) / (dy1/dx1))
	C1 = ((ax1-cx1) - (ay1-cy1) / (dy1/dx1))^2 - cr1^2
	det=B1^2 - 4 * A1 * C1
	intersection="outside"
	If dy1=0 Then
		If ay1-cy1<=cr2 Then
			intersection="inside"
			fym1=(ay1-cy1)
			fym2=(ay1-cy1)
			fxm1= cr1 * Sqr(1 - ((ay1-cy1) / cr2)^2)
			fxm2= -fxm1
		End if
	Else
		If det>0 Then
			intersection="inside"
			det=Abs(det)
			fym1 = (-B1 + Sqr(det)) / (2 * A1)
			fym2 = (-B1 - Sqr(det)) / (2 * A1)
			fxm1 = (fym1 - (ay1-cy1)) / (dy1/dx1) + (ax1-cx1)
			fxm2 = (fym2 - (ay1-cy1)) / (dy1/dx1) + (ax1-cx1)
		EndIf
	End If

	If intersection="inside" Then
		fxm1=fxm1+cx1
		fxm2=fxm2+cx1
		fym1=fym1+cy1
		fym2=fym2+cy1
		'unrotate it if ellipse was rotated
		If erot<>0 Then
			x1p=fxm1-cx1
			y1p=fym1-cy1
			fxm1=x1p*cos(erot*d2r) - y1p*sin(erot*d2r)+cx1
			fym1=y1p*cos(erot*d2r) + x1p*sin(erot*d2r)+cy1
			x1p=fxm2-cx1
			y1p=fym2-cy1
			fxm2=x1p*cos(erot*d2r) - y1p*sin(erot*d2r)+cx1
			fym2=y1p*cos(erot*d2r) + x1p*sin(erot*d2r)+cy1
		EndIf
	End If
	'check to see which intersetion the mouse is nearest to
	if sqr((x1-fxm1)^2 + (y1-fym1)^2) < sqr((x1-fxm2)^2 + (y1-fym2)^2) Then
		fxm=fxm1
		fym=fym1
	Else
		fxm=fxm2
		fym=fym2
	End If
End Sub
Sub snaptogrid()
	Dim As Double wgx,wgy,xi,yi,dbmagp
	dbmagp=wx2-wx1
	wgx=gridxspacing*Int(wx1/gridxspacing)+gridxspacing+gridxoffset
	wgx=wgx+Int((mousex-wgx)/gridxspacing)*gridxspacing
	wgy=gridyspacing*Int(wy1/gridyspacing)+gridyspacing+gridyoffset
	wgy=wgy+Int((mousey-wgy)/gridyspacing)*gridyspacing
	For xi = wgx To wgx+gridxspacing Step gridxspacing
		For yi = wgy To wgy+gridyspacing Step gridyspacing
			'PSet(xi,yi),50
			If calcd(CDbl(mousex),CDbl(mousey),0,xi,yi,0)<dbmagp Then
				dbmagp=calcd(CDbl(mousex),CDbl(mousey),0,xi,yi,0)
				gfxm=xi
				gfym=yi
			End If
		Next
	Next
End Sub
Sub initoptionboxeswindow
	'really the scroll bar for the window
	Line (942,0)-(959,638),15,bf
	Line (942,639)-(959,639),0
	Line (944,2)-(957,15),7,bf
	Line (944,17)-(958,17),7
	Line (959,2)-(959,17),7
	Line (947,9)-(950,6),0
	Line (948,10)-(950,7),0
	Line (950,6)-(953,9),0
	Line (950,7)-(952,10),0
	
	Line (944,622)-(957,635),7,bf
	Line (944,637)-(958,637),7
	Line (959,622)-(959,637),7
	Line (947,627)-(950,630),0
	Line (948,626)-(950,629),0
	Line (950,630)-(953,627),0
	Line (950,629)-(952,626),0
	sby=19'scroll bar y
	Line (944,0+sby)-(957,6+sby),7,bf
	Line (944,8+sby)-(958,8+sby),7
	Line (959,0+sby)-(959,8+sby),7
	
End Sub
Sub movesby()
	'optionbox scroll bar
	'screenset 0,0:view:Window
	'scroll bar y
	'Line (944,0+sby)-(957,6+sby),15,bf
	'Line (944,8+sby)-(958,8+sby),15
	'Line (959,0+sby)-(959,8+sby),15
	sby=mousey-msbyoffset
	If sby<19 Then sby=19
	If sby>612 Then sby=612
	scrolloptionboxes
End Sub
Sub movesbyi(msbyi As Integer)
	'optionbox scroll bar
	'screenset 0,0:view:window
	'Line (944,0+sby)-(957,6+sby),15,bf
	'Line (944,8+sby)-(958,8+sby),15
	'Line (959,0+sby)-(959,8+sby),15
	sby=sby+msbyi
	If sby<19 Then sby=19
	If sby>612 Then sby=612
	'Line (944,0+sby)-(957,6+sby),7,bf
	'Line (944,8+sby)-(958,8+sby),7
	'Line (959,0+sby)-(959,8+sby),7
	scrolloptionboxes
End Sub
Sub initoptionboxes()
	redrawoptionboxes=false
	Dim As Integer i
	'how many boxes are there
	'what are the heights for each box
	'is the box min or maximized
	optionboxc=0
	ReDim optionbox(2,3)
	ReDim optionboxtitle(2)
	addopptionbox(100,1,1,"Selected Line")
	addopptionbox(130,1,2,"Selected Circle")
	addopptionbox(100,1,3,"Detect settings")
	For i = 1 To blockc
		addopptionbox(50,1,3,blocknames(i))
	Next
	For i = 1 To 20
		addopptionbox(50,1,0,LTrim(Str(i)))
		'optionbox(i,1)=50'boxheight
		'optionbox(i,2)=1'0=minimized 1=maximized
		'optionbox(i,3)=0'0=? 1=datagrid for lines 2=datagrid for circles
		'optionboxtitle(i)=LTrim(Str(i))
	Next

	scrolloptionboxes
End Sub
Sub addopptionbox(param1 As Integer,param2 As Integer,param3 As Integer,param4 As String)
	optionboxc=optionboxc+1
	ReDim Preserve optionbox(optionboxc,3)
	ReDim Preserve optionboxtitle(optionboxc)
	optionbox(optionboxc,1)=param1'boxheight
	optionbox(optionboxc,2)=param2'0=minimized 1=maximized
	optionbox(optionboxc,3)=param3'0=? 1=datagrid for lines 2=datagrid for circles
	optionboxtitle(optionboxc)=param4'ie. "Selected Line"
End Sub
Sub scrolloptionboxes()
	Dim As Integer i
	optionboxdetectedp=0
	obht=0
	For i = 1 To optionboxc
		'tally Option box heights
		If optionbox(i,2)=0 Then obht=obht+14
		If optionbox(i,2)=1 Then obht=obht+optionbox(i,1)
		obht=obht+2
	next
	'depending on sby (scrollbar y)
	'(801,1)-(959,639)
	
	drawoptionboxes
End Sub
Sub drawoptionboxes()
	'the scrollbar is 19 to 612
	'at 19 it's 0 percent
	'at 612 it's 100 percent
	'depending on sby (scrollbar y)
	'ScreenRes 960,696,,4
	'View (0,0)-(960,639)
	Line (803,0)-(940,639),0,bf
	Line (940,0)-(940,639),13
	Line (941,0)-(941,639),0
	Line (942,0)-(959,638),15,bf
	Line (942,639)-(959,639),0
	Line (944,2)-(957,15),7,bf
	Line (944,17)-(958,17),7
	Line (959,2)-(959,17),7
	Line (947,9)-(950,6),0
	Line (948,10)-(950,7),0
	Line (950,6)-(953,9),0
	Line (950,7)-(952,10),0
	Line (944,622)-(957,635),7,bf
	Line (944,637)-(958,637),7
	Line (959,622)-(959,637),7
	Line (947,627)-(950,630),0
	Line (948,626)-(950,629),0
	Line (950,630)-(953,627),0
	Line (950,629)-(952,626),0
	Line (944,0+sby)-(957,6+sby),7,bf
	Line (944,8+sby)-(958,8+sby),7
	Line (959,0+sby)-(959,8+sby),7
	Dim As Integer i, nextboxy, dgi
	nextboxy=2-Int((obht-632)*((sby-19)/(612-19)))          '21-sby=2 (sby starts at 19)
	For i = 1 To optionboxc
		'draw optionboxes
		If optionbox(i,2)=1 Then
			'draw optionbox maximized
			'adjust starting point to draw next box
			Line (803,nextboxy+1)-(940,nextboxy+1+optionbox(i,1)),13,b
			Line (803,nextboxy+1+14)-(940,nextboxy+1+14),13',b
			'this is the min max box
			Line (930,nextboxy+3)-(940,nextboxy+12),7,bf
			'below is string for title of box
			'Draw String (805,nextboxy+5),optionboxtitle(i)
			'this is the min max button
			'Draw String (933,nextboxy+5),"-"
			'here is the data grid box
			'Line (803,nextboxy+1+14+2)-(940,nextboxy+1+optionbox(i,1)-2),15,bf
			Select case optionbox(i,3)
				Case 1'lines
					'Line (803,nextboxy+1+14+2)-(940,nextboxy+1+optionbox(i,1)-2),15,bf
					Line (803,nextboxy+1+14+2)-(940,nextboxy+1+optionbox(i,1)-2),13,b
					For dgi=1 To 6
						Line (803,nextboxy+1+14+2+14*dgi)-(940,nextboxy+1+14+2+14*dgi),13
						Select Case dgi
							Case 1
								tempstring="x1 "
								'linesingroupc,circlesingroupc,singlelinen,singlecirclen
								'If drawing=TRUE And buttonson(1)=TRUE Then
								'	tempstring=tempstring+Str(x1)
								'Else
									If selentity=TRUE And otd="line" Then
										tempstring=tempstring+Str(lines(selline,1))
									Else
										If groupexists=TRUE And linesingroupc=1 Then
											tempstring=tempstring+Str(lines(singlelinen,1))
										End If
									End If
								'End If
							Case 2
								tempstring="y1 "
								'If drawing=TRUE And buttonson(1)=TRUE Then
								'	tempstring=tempstring+Str(y1)
								'Else
									If selentity=TRUE And otd="line" Then
										tempstring=tempstring+Str(lines(selline,2))
									Else
										If groupexists=TRUE And linesingroupc=1 Then
											tempstring=tempstring+Str(lines(singlelinen,2))
										End If
									End If
								'End If
							Case 3
								tempstring="x2 "
								'If drawing=TRUE And buttonson(1)=TRUE Then
								'	tempstring=tempstring+Str(x2)
								'Else
									If selentity=TRUE And otd="line" Then
										tempstring=tempstring+Str(lines(selline,4))
									Else
										If groupexists=TRUE And linesingroupc=1 Then
											tempstring=tempstring+Str(lines(singlelinen,4))
										End If
									End If
								'End If
							Case 4
								tempstring="y2 "
								'If drawing=TRUE And buttonson(1)=TRUE Then
								'	tempstring=tempstring+Str(y2)
								'Else
									If selentity=TRUE And otd="line" Then
										tempstring=tempstring+Str(lines(selline,5))
									Else
										If groupexists=TRUE And linesingroupc=1 Then
											tempstring=tempstring+Str(lines(singlelinen,5))
										End If
									End If
								'End If
							Case 5
								tempstring=" L "
								'If drawing=TRUE And buttonson(1)=TRUE Then
								'	tempstring=tempstring+Str(length)
								'Else
									If selentity=TRUE And otd="line" Then
										tempstring=tempstring+Str(sellength)
									Else
										If groupexists=TRUE And linesingroupc=1 Then
											tempstring=tempstring+Str(sellength)
										End If
									End If
								'End If
							Case 6
								tempstring=" A "
								'If drawing=TRUE And buttonson(1)=TRUE Then
								'	tempstring=tempstring+Str(angle)
								'Else
									If selentity=TRUE And otd="line" Then
										tempstring=tempstring+Str(selangle)
									Else
										If groupexists=TRUE And linesingroupc=1 Then
											tempstring=tempstring+Str(selangle)
										End If
									End If
								'End If
						End Select
						If Len(tempstring)>17 Then tempstring=Mid(tempstring,1,17)
						'Draw String (805,nextboxy+1+14+2+14*dgi-10),tempstring
					Next
					Line (823,nextboxy+1+14+2)-(823,nextboxy+1+14+2+14*(dgi-1)),13
				Case 2'circles
					Line (803,nextboxy+1+14+2)-(940,nextboxy+1+optionbox(i,1)-2),13,b
					For dgi=1 To 7
						Line (803,nextboxy+1+14+2+14*dgi)-(940,nextboxy+1+14+2+14*dgi),13
						Select Case dgi
							Case 1
								tempstring="x1 "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","arc","ellipse","elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,1))
									End Select
									'tempstring=tempstring+Str(circles(selcircle,1))
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										tempstring=tempstring+Str(circles(singlecirclen,1))
									End If
								End If
							Case 2
								tempstring="y1 "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","arc","ellipse","elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,2))
									End Select
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										tempstring=tempstring+Str(circles(singlecirclen,2))
									End If
								End If
			'1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
							Case 3
								tempstring="r1 "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","arc","ellipse","elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,4))
									End Select
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										tempstring=tempstring+Str(circles(singlecirclen,4))
									End If
								End If
							Case 4
								tempstring="r2 "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","arc"
											tempstring=tempstring+"n/a"
										Case "ellipse","elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,8))
									End Select
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										Select Case circles(singlecirclen,9)
											Case 1,2'"circle","arc"
												tempstring=tempstring+"n/a"
											Case 3,4'"ellipse","elliptical arc"
												tempstring=tempstring+Str(circles(singlecirclen,8))
										End Select
									End If
								End If
							Case 5
								tempstring="As "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","ellipse"
											tempstring=tempstring+"n/a"
										Case "arc"
											tempstring=tempstring+Str(circles(selcircle,6)*180/pi)
										Case "elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,6))
									End Select
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										Select Case circles(singlecirclen,9)
											Case 1,3'"circle","ellipse"
												tempstring=tempstring+"n/a"
											Case 2'"arc"
												tempstring=tempstring+Str(circles(singlecirclen,6)*180/pi)
											Case 4'"elliptical arc"
												tempstring=tempstring+Str(circles(singlecirclen,6))
										End Select
									End If
								End If
							Case 6
								tempstring="Ae "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","ellipse"
											tempstring=tempstring+"n/a"
										Case "arc"
											tempstring=tempstring+Str(circles(selcircle,7)*180/pi)
										Case "elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,7))
									End Select
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										Select Case circles(singlecirclen,9)
											Case 1,3'"circle","ellipse"
												tempstring=tempstring+"n/a"
											Case 2'"arc"
												tempstring=tempstring+Str(circles(singlecirclen,7)*180/pi)
											Case 4'"elliptical arc"
												tempstring=tempstring+Str(circles(singlecirclen,7))
										End Select
									End If
								End If
							Case 7
								tempstring="Er "
								If selentity=TRUE Then
									Select Case otd
										Case "circle","arc"
											tempstring=tempstring+"n/a"
										Case "ellipse","elliptical arc"
											tempstring=tempstring+Str(circles(selcircle,11))
									End Select
								Else
									If groupexists=TRUE And circlesingroupc=1 Then
										Select Case circles(singlecirclen,9)
											Case 1,2'"circle","arc"
												tempstring=tempstring+"n/a"
											Case 3,4'"ellipse","elliptical arc"
												tempstring=tempstring+Str(circles(singlecirclen,11))
										End Select
									End If
								End If
						End Select
						'Draw String (805,nextboxy+1+14+2+14*dgi-10),tempstring
					Next
					Line (823,nextboxy+1+14+2)-(823,nextboxy+1+14+2+14*(dgi-1)),13
				Case 3'blocks
					Line (803,nextboxy+1+14+2)-(940,nextboxy+1+optionbox(i,1)-2),13,b
					For dgi=1 To 3'filtertypes
						Line (803,nextboxy+1+14+2+14*dgi)-(940,nextboxy+1+14+2+14*dgi),13
						'Draw String (807,nextboxy+1+14+2+14*dgi-10),filtertype(dgi,1)
						'Draw String (821,nextboxy+1+14+2+14*dgi-10),filtertype(dgi,2)
					Next
					Line (817,nextboxy+1+14+2)-(817,nextboxy+1+14+2+14*(dgi-1)),13
			End Select
			nextboxy=nextboxy+optionbox(i,1)+2
		Else
			'draw optionbox minimized
			'adjust starting point to draw next box
			Line (803,nextboxy+1)-(940,nextboxy+1+14),14,b
			Line (930,nextboxy+3)-(940,nextboxy+1+12),7,bf
			'below is string for title of box
			'Draw String (805,nextboxy+5),optionboxtitle(i)
			'Draw String (933,nextboxy+5),"+"
			nextboxy=nextboxy+17
		EndIf
	Next
	'View
	Line (942,0)-(959,638),15,bf
	Line (940,0)-(940,639),13
	Line (941,0)-(941,639),0
	Line (942,639)-(959,639),0
	Line (944,2)-(957,15),7,bf
	Line (944,17)-(958,17),7
	Line (959,2)-(959,17),7
	Line (947,9)-(950,6),0
	Line (948,10)-(950,7),0
	Line (950,6)-(953,9),0
	Line (950,7)-(952,10),0
	Line (944,622)-(957,635),7,bf
	Line (944,637)-(958,637),7
	Line (959,622)-(959,637),7
	Line (947,627)-(950,630),0
	Line (948,626)-(950,629),0
	Line (950,630)-(953,627),0
	Line (950,629)-(952,626),0
	Line (944,0+sby)-(957,6+sby),7,bf
	Line (944,8+sby)-(958,8+sby),7
	Line (959,0+sby)-(959,8+sby),7
End Sub
Sub detectoptionboxes()
	'i think this just detects which option box the mouse is in
	'the scrollbar is 19 to 612
	'at 19 it's 0 percent
	'at 612 it's 100 percent
	
	'depending on sby (scrollbar y)
	Dim As Integer i, nextboxy
	nextboxy=2-Int((obht-632)*((sby-19)/(612-19)))          '21-sby=2 (sby starts at 19)
	For i = 1 To optionboxc
		'draw optionboxes
		If optionbox(i,2)=1 Then
			Select Case mousey
				Case nextboxy+1 To nextboxy+1+optionbox(i,1)
					optionboxdetected=i
					activeoptionboxx1=803
					activeoptionboxy1=nextboxy+1
					activeoptionboxx2=940
					activeoptionboxy2=nextboxy+1+optionbox(i,1)
					activeoptionboxminmaxx1=930
					activeoptionboxminmaxy1=nextboxy+3
					activeoptionboxminmaxx2=940
					activeoptionboxminmaxy2=nextboxy+1+12
					Exit sub
			End Select
'			'draw optionbox maximized
'			'adjust starting point to draw next box
'			Line (803,nextboxy+1)-(940,nextboxy+1+optionbox(i,1)),13,b
'			Line (803,nextboxy+1+14)-(940,nextboxy+1+14),13,b
'			'this is the min max box
'			Line (930,nextboxy+3)-(940,nextboxy+12),7,bf
'			'below is string for title of box
'			Draw String (805,nextboxy+5),LTrim(Str(i))
'			'this is the min max button
'			Draw String (933,nextboxy+5),"-"
'			'here is the data grid box
'			Line (803,nextboxy+1+14+2)-(940,nextboxy+1+optionbox(i,1)-2),15,bf
			nextboxy=nextboxy+optionbox(i,1)+2
		Else
			Select Case mousey
				Case nextboxy+1 To nextboxy+1+14
					optionboxdetected=i
					activeoptionboxx1=803
					activeoptionboxy1=nextboxy+1
					activeoptionboxx2=940
					activeoptionboxy2=nextboxy+1+14
					activeoptionboxminmaxx1=930
					activeoptionboxminmaxy1=nextboxy+3
					activeoptionboxminmaxx2=940
					activeoptionboxminmaxy2=nextboxy+1+12
					Exit sub
			End Select
'			'draw optionbox minimized
'			'adjust starting point to draw next box
'			Line (803,nextboxy+1)-(940,nextboxy+1+14),14,b
'			Line (930,nextboxy+3)-(940,nextboxy+1+12),7,bf
'			Draw String (805,nextboxy+5),LTrim(Str(i))
'			Draw String (933,nextboxy+5),"+"
			nextboxy=nextboxy+17
		EndIf
	Next
End Sub
Sub datagrid(dgtop As Integer,dgleft As Integer,dghight As Integer,dgwidth As Integer,dgcolumbsc As Integer,dgrowsc As Integer)
	
End Sub
Sub monkeysmatter(mmrefbi As Integer, mmbi As Integer)
'1x,2y,3z,4r,5color,6start,7end,8aspect,
'9=1(circle)2(arc)3(ellips)4(ellipticalarc),
'10group,11 is ellipse rotation , 12 block
'with i equal to an angle x,y is calcd
'eplotx=Cos(i*d2r)*er1*Cos(eangle*d2r) - Sin(i*d2r)*er2*Sin(eangle*d2r)+ex1
'eploty=Cos(i*d2r)*er1*Sin(eangle*d2r) + Sin(i*d2r)*er2*Cos(eangle*d2r)+ey1

'mmbi is the current block number used in blockoffsets
'selcircle is used to identify which ellipse

'identifiy the 4 xy (end points) of the major and minor axis
'scale these 4 points along with the center point
'according to blockoffsets
'A is at 180
'B is at 0 or 360
'c is the center
'D is at 90
'E is at 270
	Dim As Double er1,er2,eangle,ex1,ey1
	er1=circles(selcircle,4)
	er2=circles(selcircle,8)
	eangle=circles(selcircle,11)
	ex1=circles(selcircle,1)
	ey1=circles(selcircle,2)
	Dim As Double mmax,mmbx,mmcx,mmdx,mmex,mmkx,mmmx,mmnx,mmnnx,tempmmnnx,mmox,tempmmox,mmpx,tempmmpx,mmrx,mmtx,tempmmtx,mmttx
	Dim As Double mmay,mmby,mmcy,mmdy,mmey,mmky,mmmy,mmny,mmnny,tempmmnny,mmoy,tempmmoy,mmpy,tempmmpy,mmry,mmty,tempmmty,mmtty
	
	mmax=Cos(180*d2r)*er1*Cos(eangle*d2r) - Sin(180*d2r)*er2*Sin(eangle*d2r)+ex1
	mmay=Cos(180*d2r)*er1*Sin(eangle*d2r) + Sin(180*d2r)*er2*Cos(eangle*d2r)+ey1
	mmbx=Cos(360*d2r)*er1*Cos(eangle*d2r) - Sin(360*d2r)*er2*Sin(eangle*d2r)+ex1
	mmby=Cos(360*d2r)*er1*Sin(eangle*d2r) + Sin(360*d2r)*er2*Cos(eangle*d2r)+ey1
	mmcx=ex1
	mmcy=ey1
	mmdx=Cos(90*d2r)*er1*Cos(eangle*d2r) - Sin(90*d2r)*er2*Sin(eangle*d2r)+ex1
	mmdy=Cos(90*d2r)*er1*Sin(eangle*d2r) + Sin(90*d2r)*er2*Cos(eangle*d2r)+ey1
	mmex=Cos(270*d2r)*er1*Cos(eangle*d2r) - Sin(270*d2r)*er2*Sin(eangle*d2r)+ex1
	mmey=Cos(270*d2r)*er1*Sin(eangle*d2r) + Sin(270*d2r)*er2*Cos(eangle*d2r)+ey1

	'	now scalse A-E to blockoffsets
	'A=A*scale
	'B=B*scalse
	'C=C*scale
	'D=D*scalse
	'E=E*scalse
	'lines(j,1)=blockoffsets(blockc,1)+(lines(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
	mmax=blockoffsets(mmbi,1)+(mmax -blockoffsets(mmbi,1))*blockoffsets(mmbi,4)
	mmay=blockoffsets(mmbi,2)+(mmay-blockoffsets(mmbi,2))*blockoffsets(mmbi,5)
	mmbx=blockoffsets(mmbi,1)+(mmbx -blockoffsets(mmbi,1))*blockoffsets(mmbi,4)
	mmby=blockoffsets(mmbi,2)+(mmby-blockoffsets(mmbi,2))*blockoffsets(mmbi,5)
	mmcx=blockoffsets(mmbi,1)+(mmcx -blockoffsets(mmbi,1))*blockoffsets(mmbi,4)
	mmcy=blockoffsets(mmbi,2)+(mmcy-blockoffsets(mmbi,2))*blockoffsets(mmbi,5)
	mmdx=blockoffsets(mmbi,1)+(mmdx -blockoffsets(mmbi,1))*blockoffsets(mmbi,4)
	mmdy=blockoffsets(mmbi,2)+(mmdy-blockoffsets(mmbi,2))*blockoffsets(mmbi,5)
	mmex=blockoffsets(mmbi,1)+(mmex -blockoffsets(mmbi,1))*blockoffsets(mmbi,4)
	mmey=blockoffsets(mmbi,2)+(mmey-blockoffsets(mmbi,2))*blockoffsets(mmbi,5)

'now that the axis are scaled, they are no longer the major and minor axis
'instead they are conjugate diameters.
'conjugate diameter are tangent bisectors.
'the goal is to derive what the major and minor axis are
'with only 3 pieces of information
'the 2 conjucate diamters and the center of the ellipse 
	xlength=mmbx-mmcx
	ylength=mmby-mmcy
	fixangle
	angle=angle-90
	If angle<0 then angle=360+angle
	length = sqr((mmcx-mmbx)^2 + (mmcy-mmby)^2)
	mmkx=mmex+cos(angle*d2r)*length
	mmky=mmey+sin(angle*d2r)*length
	angle=angle+90
	length=1000'just a temp length in order to calc intersection
	tempmmtx=mmex+cos(angle*d2r)*length
	tempmmty=mmey+sin(angle*d2r)*length
	mmrx=(mmcx+mmkx)/2
	mmry=(mmcy+mmky)/2
	xlength=mmkx-mmcx
	ylength=mmky-mmcy
	fixangle
	angle=angle+90
	If angle<0 then angle=360+angle
	length=1000'just a temp length in order to calc intersection
	tempmmox=mmrx+cos(angle*d2r)*length
	tempmmoy=mmry+sin(angle*d2r)*length
'	cald intersectoin between vectors R,tempO and E,tempT
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,cx1,cy1,cr1
	Dim As Double A1,A2,B1,B2,C1,C2,det,dx1,dy1
	ax1=mmrx
	ay1=mmry
	ax2=tempmmox
	ay2=tempmmoy
	bx1=mmex
	by1=mmey
	bx2=tempmmtx
	by2=tempmmty
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	mmox = (B2*C1 - B1*C2)/det
	mmoy = (A1*C2 - A2*C1)/det
	'ok is radius of circle
'	Dim As Double ax1,ay1,ax2,ay2,cx1,cy1,cr1
'	Dim As Double A1,B1,C1,det,dx1,dy1,fxm1,fxm2,fym1,fym2,aia
'	Dim As Integer i
	ax1=mmex
	ay1=mmey
	ax2=tempmmtx
	ay2=tempmmty
	cx1=mmox
	cy1=mmoy
	cr1=sqr((mmkx-mmox)^2 + (mmky-mmoy)^2)
	dx1 = ax2 - ax1
	dy1 = ay2 - ay1
	
	A1 = dx1^2 + dy1^2
	B1 = 2 * (dx1 * (ax1 - cx1) + dy1 * (ay1 - cy1))
	C1 = (ax1 - cx1)^2 + (ay1 - cy1)^2 - cr1^2
	
	det = B1 * B1 - 4 * A1 * C1
	det=Abs(det)
	
	'one point
	mmtx = ax1 + ((-B1 + Sqr(det)) / (2 * A1)) * dx1
	mmty = ay1 + ((-B1 + Sqr(det)) / (2 * A1)) * dy1
	'other point
	mmttx = ax1 + ((-B1 - Sqr(det)) / (2 * A1)) * dx1
	mmtty = ay1 + ((-B1 - Sqr(det)) / (2 * A1)) * dy1
	
	'n And m
	xlength=mmtx-mmkx
	ylength=mmty-mmky
	fixangle
	length=sqr((mmkx-mmex)^2 + (mmky-mmey)^2)
	mmnx=mmkx+Cos(angle*d2r)*length
	mmny=mmky+sin(angle*d2r)*length
	
	xlength=mmttx-mmkx
	ylength=mmtty-mmky
	fixangle
	length=sqr((mmkx-mmex)^2 + (mmky-mmey)^2)
	mmmx=mmkx+Cos(angle*d2r)*length
	mmmy=mmky+sin(angle*d2r)*length
	
	xlength=mmcx-mmkx
	ylength=mmcy-mmky
	fixangle
	length=1000'just a temp length in order to calc intersection
	tempmmnnx=mmnx+Cos(angle*d2r)*length
	tempmmnny=mmny+sin(angle*d2r)*length
	tempmmpx=mmmx+Cos(angle*d2r)*length
	tempmmpy=mmmy+sin(angle*d2r)*length
	
	'calc intersections for nn and p
	ax1=mmnx
	ay1=mmny
	ax2=tempmmnnx
	ay2=tempmmnny
	bx1=mmcx
	by1=mmcy
	bx2=mmtx
	by2=mmty
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	mmnnx = (B2*C1 - B1*C2)/det
	mmnny = (A1*C2 - A2*C1)/det
	
	ax1=mmmx
	ay1=mmmy
	ax2=tempmmpx
	ay2=tempmmpy
	bx1=mmcx
	by1=mmcy
	bx2=mmttx
	by2=mmtty
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	mmpx = (B2*C1 - B1*C2)/det
	mmpy = (A1*C2 - A2*C1)/det
	
	'c-nn is the major axes radius
	'c-p is the minor axes radius
	'er1=circles(selcircle,4)
	'er2=circles(selcircle,8)
	'eangle=circles(selcircle,11)
	circles(selcircle,4)=sqr((mmcx-mmnnx)^2 + (mmcy-mmnny)^2)
	circles(selcircle,8)=sqr((mmcx-mmpx)^2 + (mmcy-mmpy)^2)
	xlength=mmnnx-mmcx
	ylength=mmnny-mmcy
	fixangle
	circles(selcircle,11)=angle
	'ex1=circles(selcircle,1)
	'ey1=circles(selcircle,2)
	circles(selcircle,1)=blockoffsets(mmbi,1)+(ex1 -blockoffsets(mmbi,1))*blockoffsets(mmbi,4)
	circles(selcircle,2)=blockoffsets(mmbi,2)+(ey1-blockoffsets(mmbi,2))*blockoffsets(mmbi,5)
	If circles(selcircle,9)=4 Then'if it's an elliptical arc
		'find angle of point B (mmbx,y)
		'add this angle to the start and end angles of the arc
		x1=mmcx
		y1=mmcy
		x2=mmbx
		y2=mmby
		erotation=circles(selcircle,11)
		radius=circles(selcircle,4)
		eradius=circles(selcircle,8)
		x1p=x2-x1
		y1p=(y2-y1)
		x2=x1p*Cos(-erotation*d2r) - y1p*Sin(-erotation*d2r)+x1
		y2=y1p*Cos(-erotation*d2r) + x1p*Sin(-erotation*d2r)+y1
		ylength=(y2-y1)*(radius/eradius)
		xlength=x2-x1
		fixangle
		circles(selcircle,6)=circles(selcircle,6)+angle
		circles(selcircle,7)=circles(selcircle,7)+angle
	EndIf
End Sub
Sub monkeysmatter2(mmbi1 As Double,mmbi2 As Double,mmbi4 As Double,mmbi5 As Double)

'due to an error when mmbi4 (scalex) or mmbi5 (scaley) is negative
'i had to impliment a flipping routine at the bottom
'i use variables flipithorz and flipitvert
Dim As BOOLEAN flipithorz ,flipitvert
flipithorz=FALSE
flipitvert=FALSE
If mmbi4<0 Then flipithorz=TRUE
If mmbi5<0 Then flipitvert=TRUE
'force mmbi4 and 5 to abs
mmbi4=Abs(mmbi4)
mmbi5=Abs(mmbi5)


'1x,2y,3z,4r,5color,6start,7end,8aspect,
'9=1(circle)2(arc)3(ellips)4(ellipticalarc),
'10group,11 is ellipse rotation , 12 block
'with i equal to an angle x,y is calcd
'eplotx=Cos(i*d2r)*er1*Cos(eangle*d2r) - Sin(i*d2r)*er2*Sin(eangle*d2r)+ex1
'eploty=Cos(i*d2r)*er1*Sin(eangle*d2r) + Sin(i*d2r)*er2*Cos(eangle*d2r)+ey1

'mmbi is the current block number used in blockoffsets
'selcircle is used to identify which ellipse

'identifiy the 4 xy (end points) of the major and minor axis
'scale these 4 points along with the center point
'according to blockoffsets
'A is at 180
'B is at 0 or 360
'c is the center
'D is at 90
'E is at 270
	Dim As Double er1,er2,eangle,ex1,ey1
	er1=circles(selcircle,4)
	er2=circles(selcircle,8)
	eangle=circles(selcircle,11)
	ex1=circles(selcircle,1)
	ey1=circles(selcircle,2)
	Dim As Double mmax,mmbx,mmcx,mmdx,mmex,mmkx,mmmx,mmnx,mmnnx,tempmmnnx,mmox,tempmmox,mmpx,tempmmpx,mmrx,mmtx,tempmmtx,mmttx
	Dim As Double mmay,mmby,mmcy,mmdy,mmey,mmky,mmmy,mmny,mmnny,tempmmnny,mmoy,tempmmoy,mmpy,tempmmpy,mmry,mmty,tempmmty,mmtty
	
	mmax=Cos(180*d2r)*er1*Cos(eangle*d2r) - Sin(180*d2r)*er2*Sin(eangle*d2r)+ex1
	mmay=Cos(180*d2r)*er1*Sin(eangle*d2r) + Sin(180*d2r)*er2*Cos(eangle*d2r)+ey1
	mmbx=Cos(360*d2r)*er1*Cos(eangle*d2r) - Sin(360*d2r)*er2*Sin(eangle*d2r)+ex1
	mmby=Cos(360*d2r)*er1*Sin(eangle*d2r) + Sin(360*d2r)*er2*Cos(eangle*d2r)+ey1
	mmcx=ex1
	mmcy=ey1
	mmdx=Cos(90*d2r)*er1*Cos(eangle*d2r) - Sin(90*d2r)*er2*Sin(eangle*d2r)+ex1
	mmdy=Cos(90*d2r)*er1*Sin(eangle*d2r) + Sin(90*d2r)*er2*Cos(eangle*d2r)+ey1
	mmex=Cos(270*d2r)*er1*Cos(eangle*d2r) - Sin(270*d2r)*er2*Sin(eangle*d2r)+ex1
	mmey=Cos(270*d2r)*er1*Sin(eangle*d2r) + Sin(270*d2r)*er2*Cos(eangle*d2r)+ey1

	'	now scalse A-E to blockoffsets
	'A=A*scale
	'B=B*scalse
	'C=C*scale
	'D=D*scalse
	'E=E*scalse
	'lines(j,1)=blockoffsets(blockc,1)+(lines(j,1)-blockoffsets(blockc,1))*blockoffsets(blockc,4)
	mmax=mmbi1+(mmax -mmbi1)*mmbi4
	mmay=mmbi2+(mmay-mmbi2)*mmbi5
	mmbx=mmbi1+(mmbx -mmbi1)*mmbi4
	mmby=mmbi2+(mmby-mmbi2)*mmbi5
	mmcx=mmbi1+(mmcx -mmbi1)*mmbi4
	mmcy=mmbi2+(mmcy-mmbi2)*mmbi5
	mmdx=mmbi1+(mmdx -mmbi1)*mmbi4
	mmdy=mmbi2+(mmdy-mmbi2)*mmbi5
	mmex=mmbi1+(mmex -mmbi1)*mmbi4
	mmey=mmbi2+(mmey-mmbi2)*mmbi5

'now that the axis are scaled, they are no longer the major and minor axis
'instead they are conjugate diameters.
'conjugate diameter are tangent bisectors.
'the goal is to derive what the major and minor axis are
'with only 3 pieces of information
'the 2 conjucate diamters and the center of the ellipse 
'
'	mmkx=
'	mmky=
	xlength=mmbx-mmcx
	ylength=mmby-mmcy
	fixangle
	angle=angle-90
	If angle<0 then angle=360+angle
	length = sqr((mmcx-mmbx)^2 + (mmcy-mmby)^2)
	mmkx=mmex+cos(angle*d2r)*length
	mmky=mmey+sin(angle*d2r)*length
	angle=angle+90
	length=1000'just a temp length in order to calc intersection
	tempmmtx=mmex+cos(angle*d2r)*length
	tempmmty=mmey+sin(angle*d2r)*length
'	
'	mmrx=mid point between CK
'	mmry=mid point between CK
	mmrx=(mmcx+mmkx)/2
	mmry=(mmcy+mmky)/2
	xlength=mmkx-mmcx
	ylength=mmky-mmcy
	fixangle
	angle=angle+90
	If angle<0 then angle=360+angle
	length=1000'just a temp length in order to calc intersection
	tempmmox=mmrx+cos(angle*d2r)*length
	tempmmoy=mmry+sin(angle*d2r)*length
'	cald intersectoin between vectors R,tempO and E,tempT
	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2,cx1,cy1,cr1
	Dim As Double A1,A2,B1,B2,C1,C2,det,dx1,dy1
	ax1=mmrx
	ay1=mmry
	ax2=tempmmox
	ay2=tempmmoy
	bx1=mmex
	by1=mmey
	bx2=tempmmtx
	by2=tempmmty
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	mmox = (B2*C1 - B1*C2)/det
	mmoy = (A1*C2 - A2*C1)/det
	
	ax1=mmex
	ay1=mmey
	ax2=tempmmtx
	ay2=tempmmty
	cx1=mmox
	cy1=mmoy
	cr1=sqr((mmkx-mmox)^2 + (mmky-mmoy)^2)
	dx1 = ax2 - ax1
	dy1 = ay2 - ay1
	
	A1 = dx1^2 + dy1^2
	B1 = 2 * (dx1 * (ax1 - cx1) + dy1 * (ay1 - cy1))
	C1 = (ax1 - cx1)^2 + (ay1 - cy1)^2 - cr1^2
	
	det = B1 * B1 - 4 * A1 * C1
	det=Abs(det)
	
	'one point
	mmtx = ax1 + ((-B1 + Sqr(det)) / (2 * A1)) * dx1
	mmty = ay1 + ((-B1 + Sqr(det)) / (2 * A1)) * dy1
	'other point
	mmttx = ax1 + ((-B1 - Sqr(det)) / (2 * A1)) * dx1
	mmtty = ay1 + ((-B1 - Sqr(det)) / (2 * A1)) * dy1
	
	'n And m
	xlength=mmtx-mmkx
	ylength=mmty-mmky
	fixangle
	length=sqr((mmkx-mmex)^2 + (mmky-mmey)^2)
	mmnx=mmkx+Cos(angle*d2r)*length
	mmny=mmky+sin(angle*d2r)*length
	
	xlength=mmttx-mmkx
	ylength=mmtty-mmky
	fixangle
	length=sqr((mmkx-mmex)^2 + (mmky-mmey)^2)
	mmmx=mmkx+Cos(angle*d2r)*length
	mmmy=mmky+sin(angle*d2r)*length
	
	xlength=mmcx-mmkx
	ylength=mmcy-mmky
	fixangle
	length=1000'just a temp length in order to calc intersection
	tempmmnnx=mmnx+Cos(angle*d2r)*length
	tempmmnny=mmny+sin(angle*d2r)*length
	tempmmpx=mmmx+Cos(angle*d2r)*length
	tempmmpy=mmmy+sin(angle*d2r)*length
	
	'calc intersections for nn and p
	ax1=mmnx
	ay1=mmny
	ax2=tempmmnnx
	ay2=tempmmnny
	bx1=mmcx
	by1=mmcy
	bx2=mmtx
	by2=mmty
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	mmnnx = (B2*C1 - B1*C2)/det
	mmnny = (A1*C2 - A2*C1)/det
	
	ax1=mmmx
	ay1=mmmy
	ax2=tempmmpx
	ay2=tempmmpy
	bx1=mmcx
	by1=mmcy
	bx2=mmttx
	by2=mmtty
	A1 = ay2-ay1
	B1 = ax1-ax2
	C1 = A1*ax1+B1*ay1
	A2 = by2-by1
	B2 = bx1-bx2
	C2 = A2*bx1+B2*by1
	det = A1*B2 - A2*B1
	mmpx = (B2*C1 - B1*C2)/det
	mmpy = (A1*C2 - A2*C1)/det
	
	'c-nn is the major axes radius
	'c-p is the minor axes radius
	'er1=circles(selcircle,4)
	'er2=circles(selcircle,8)
	'eangle=circles(selcircle,11)
	circles(selcircle,4)=sqr((mmcx-mmnnx)^2 + (mmcy-mmnny)^2)
	circles(selcircle,8)=sqr((mmcx-mmpx)^2 + (mmcy-mmpy)^2)
	xlength=mmnnx-mmcx
	ylength=mmnny-mmcy
	fixangle
	circles(selcircle,11)=angle
	'ex1=circles(selcircle,1)
	'ey1=circles(selcircle,2)
	circles(selcircle,1)=mmbi1+(ex1 -mmbi1)*mmbi4
	circles(selcircle,2)=mmbi2+(ey1-mmbi2)*mmbi5
	If circles(selcircle,9)=4 Then'if it's an elliptical arc
		'find angle of point B (mmbx,y)
		'add this angle to the start and end angles of the arc
		x1=mmcx
		y1=mmcy
		x2=mmbx
		y2=mmby
		erotation=circles(selcircle,11)
		radius=circles(selcircle,4)
		eradius=circles(selcircle,8)
		x1p=x2-x1
		y1p=(y2-y1)
		x2=x1p*Cos(-erotation*d2r) - y1p*Sin(-erotation*d2r)+x1
		y2=y1p*Cos(-erotation*d2r) + x1p*Sin(-erotation*d2r)+y1
		ylength=(y2-y1)*(radius/eradius)
		xlength=x2-x1
		fixangle
		circles(selcircle,6)=circles(selcircle,6)+angle
		circles(selcircle,7)=circles(selcircle,7)+angle
	EndIf
	
	
	
	Dim As Integer i
	i=selcircle
	If flipithorz=TRUE Then
		mmbi4=mmbi4*-1
		circles(selcircle,1)=mmbi1+(ex1 -mmbi1)*mmbi4
		tempdouble=360-circles(i,7)-circles(i,6)
		circles(i,6)=circles(i,6)+tempdouble+180
		circles(i,7)=circles(i,7)+tempdouble+180
		circles(i,6)=mymod(circles(i,6),360)
		circles(i,7)=mymod(circles(i,7),360)
		circles(i,11)=360-circles(i,11)
		circles(i,11)=mymod(circles(i,11),360)
	EndIf
	If flipitvert=TRUE Then
		mmbi5=mmbi5*-1
		circles(selcircle,2)=mmbi2+(ey1-mmbi2)*mmbi5
		tempdouble=360-circles(i,7)-circles(i,6)
		circles(i,6)=circles(i,6)+tempdouble
		circles(i,7)=circles(i,7)+tempdouble
		circles(i,6)=mymod(circles(i,6),360)
		circles(i,7)=mymod(circles(i,7),360)
		circles(i,11)=360-circles(i,11)
		circles(i,11)=mymod(circles(i,11),360)
	EndIf
End Sub
Sub scalegroup()
	Dim As Integer i,j
	Dim As Double tempscalexfactor, tempscaleyfactor, tempscalezfactor
	If shift_key=1 Or shift_key=2 Then
		insertionx=groupgrabhandlexy(9,1)
		insertiony=groupgrabhandlexy(9,2)
	Else
		Select Case groupgrabhandle
			Case "top left"'1
				insertionx=groupgrabhandlexy(5,1)
				insertiony=groupgrabhandlexy(5,2)
			Case "top mid"'2
				insertionx=groupgrabhandlexy(6,1)
				insertiony=groupgrabhandlexy(6,2)
			Case "top right"'3
				insertionx=groupgrabhandlexy(7,1)
				insertiony=groupgrabhandlexy(7,2)
			Case "right side mid"'4
				insertionx=groupgrabhandlexy(8,1)
				insertiony=groupgrabhandlexy(8,2)
			Case "bottom right"'5
				insertionx=groupgrabhandlexy(1,1)
				insertiony=groupgrabhandlexy(1,2)
			Case "bottom mid"'6
				insertionx=groupgrabhandlexy(2,1)
				insertiony=groupgrabhandlexy(2,2)
			Case "bottom left"'7
				insertionx=groupgrabhandlexy(3,1)
				insertiony=groupgrabhandlexy(3,2)
			Case "left side mid"'8
				insertionx=groupgrabhandlexy(4,1)
				insertiony=groupgrabhandlexy(4,2)
		End Select
	End If
	tempint=0
	For i = tempgrouplinec To linec
		tempint=tempint+1
		For j=1 To 7
			lines(i,j)=lines(grouplines(tempint),j)
		Next
	Next
	tempint=0
	For i = tempgroupcirclec To circlec
		tempint=tempint+1
		For j=1 To 9
			circles(i,j)=circles(groupcircles(tempint),j)
		Next
		circles(i,11)=circles(groupcircles(tempint),11)
	Next
	If shift_key=1 Or shift_key=2 Then
		Select Case groupgrabhandle'scale to center
			Case "top left"'1
				tempscalexfactor=(groupgrabhandlexy(9,1)-mousex)/(groupgrabhandlexy(9,1)-groupgrabhandlexy(8,1))
				tempscaleyfactor=(mousey-groupgrabhandlexy(9,2))/(groupgrabhandlexy(2,2)-groupgrabhandlexy(9,2))
			Case "top mid"'2
				tempscalexfactor=1
				tempscaleyfactor=(mousey-groupgrabhandlexy(9,2))/(groupgrabhandlexy(2,2)-groupgrabhandlexy(9,2))
			Case "top right"'3
				tempscalexfactor=(mousex-groupgrabhandlexy(9,1))/(groupgrabhandlexy(4,1)-groupgrabhandlexy(9,1))
				tempscaleyfactor=(mousey-groupgrabhandlexy(9,2))/(groupgrabhandlexy(2,2)-groupgrabhandlexy(9,2))
			Case "right side mid"'4
				tempscalexfactor=(mousex-groupgrabhandlexy(9,1))/(groupgrabhandlexy(4,1)-groupgrabhandlexy(9,1))
				tempscaleyfactor=1
			Case "bottom right"'5
				tempscalexfactor=(mousex-groupgrabhandlexy(9,1))/(groupgrabhandlexy(4,1)-groupgrabhandlexy(9,1))
				tempscaleyfactor=(groupgrabhandlexy(9,2)-mousey)/(groupgrabhandlexy(9,2)-groupgrabhandlexy(6,2))
			Case "bottom mid"'6
				tempscalexfactor=1
				tempscaleyfactor=(groupgrabhandlexy(9,2)-mousey)/(groupgrabhandlexy(9,2)-groupgrabhandlexy(6,2))
			Case "bottom left"'7
				tempscalexfactor=(groupgrabhandlexy(9,1)-mousex)/(groupgrabhandlexy(9,1)-groupgrabhandlexy(8,1))
				tempscaleyfactor=(groupgrabhandlexy(9,2)-mousey)/(groupgrabhandlexy(9,2)-groupgrabhandlexy(6,2))
			Case "left side mid"'8
				tempscalexfactor=(groupgrabhandlexy(9,1)-mousex)/(groupgrabhandlexy(9,1)-groupgrabhandlexy(8,1))
				tempscaleyfactor=1
		End Select
	Else
		Select Case groupgrabhandle'scale to opposite handle
			Case "top left"'1
				tempscalexfactor=(groupgrabhandlexy(5,1)-mousex)/(groupgrabhandlexy(5,1)-groupgrabhandlexy(1,1))
				tempscaleyfactor=(mousey-groupgrabhandlexy(5,2))/(groupgrabhandlexy(1,2)-groupgrabhandlexy(5,2))
			Case "top mid"'2
				tempscalexfactor=1
				tempscaleyfactor=(mousey-groupgrabhandlexy(6,2))/(groupgrabhandlexy(2,2)-groupgrabhandlexy(6,2))
			Case "top right"'3
				tempscalexfactor=(mousex-groupgrabhandlexy(7,1))/(groupgrabhandlexy(3,1)-groupgrabhandlexy(7,1))
				tempscaleyfactor=(mousey-groupgrabhandlexy(7,2))/(groupgrabhandlexy(3,2)-groupgrabhandlexy(7,2))
			Case "right side mid"'4
				tempscalexfactor=(mousex-groupgrabhandlexy(8,1))/(groupgrabhandlexy(4,1)-groupgrabhandlexy(8,1))
				tempscaleyfactor=1
			Case "bottom right"'5
				tempscalexfactor=(mousex-groupgrabhandlexy(1,1))/(groupgrabhandlexy(5,1)-groupgrabhandlexy(1,1))
				tempscaleyfactor=(groupgrabhandlexy(1,2)-mousey)/(groupgrabhandlexy(1,2)-groupgrabhandlexy(5,2))
			Case "bottom mid"'6
				tempscalexfactor=1
				tempscaleyfactor=(groupgrabhandlexy(2,2)-mousey)/(groupgrabhandlexy(2,2)-groupgrabhandlexy(6,2))
			Case "bottom left"'7
				tempscalexfactor=(groupgrabhandlexy(3,1)-mousex)/(groupgrabhandlexy(3,1)-groupgrabhandlexy(7,1))
				tempscaleyfactor=(groupgrabhandlexy(3,2)-mousey)/(groupgrabhandlexy(3,2)-groupgrabhandlexy(7,2))
			Case "left side mid"'8
				tempscalexfactor=(groupgrabhandlexy(4,1)-mousex)/(groupgrabhandlexy(4,1)-groupgrabhandlexy(8,1))
				tempscaleyfactor=1
		End Select
	End If
	'now scale them respectively
	If tempscalexfactor <> 0 Or tempscaleyfactor <> 0 Then
		If groupscaling=FALSE Then
			escapegroupscaling
			setscaledgroupdown(tempscalexfactor, tempscaleyfactor, 0)
		Else
			For i = tempgrouplinec To linec
				scaleit("LINE",i,tempscalexfactor,tempscaleyfactor,tempscalezfactor,insertionx,insertiony)
			Next
			tempint=0
			For i = tempgroupcirclec To circlec
				scaleit("CIRCLE",i,tempscalexfactor,tempscaleyfactor,tempscalezfactor,insertionx,insertiony)
			Next
			'show the scale factors
			tempstring="scale X="+Str(tempscalexfactor)
			'Locate 7,1
			'Print Left(tempstring,12);"   "
			tempstring="scale Y="+Str(tempscaleyfactor)
			'Locate 8,1
			'Print Left(tempstring,12);"   "
		End If
		'display the scaled group
		inview()
		redraw
		'showgroups
	End If
End Sub
Sub setscaledgroupdown(tempgscalexfactor As Double, tempgscaleyfactor As Double, tempgscalezfactor As Double)
	Dim As Integer i
	Dim As BOOLEAN scalingblock,scalingblocks(blockc)
	scalingblock=FALSE
	For i = 1 To linec
		If lines(i,8)=1 Then
			If lines(i,9)>0 Then
				scalingblock=TRUE
				scalingblocks(lines(i,9))=TRUE
			EndIf
			scaleit("LINE",i,tempgscalexfactor,tempgscaleyfactor,tempgscalezfactor,insertionx,insertiony)
		EndIf
	Next
	For i = 1 To circlec
		If circles(i,10)=1 Then
			If circles(i,12)>0 Then
				scalingblock=TRUE
				scalingblocks(circles(i,12))=TRUE
			EndIf
			scaleit("CIRCLE",i,tempgscalexfactor,tempgscaleyfactor,tempgscalezfactor,insertionx,insertiony)
		EndIf
	Next
	If scalingblock=TRUE Then
		For i = 1 To blockc
			If scalingblocks(i)=TRUE Then
				blockoffsets(i,1)=insertionx+(blockoffsets(i,1)-insertionx)*tempgscalexfactor
				blockoffsets(i,2)=insertiony+(blockoffsets(i,2)-insertiony)*tempgscaleyfactor
				'blockoffsets(i,3)=
				blockoffsets(i,4)=blockoffsets(i,4)*tempgscalexfactor
				blockoffsets(i,5)=blockoffsets(i,5)*tempgscaleyfactor
				blockoffsets(i,6)=0
			EndIf
		Next
	EndIf
End Sub
Sub escapegroupscaling()
	Dim As Integer i,j
	For i = tempgrouplinec To linec
		For j=1 To 9
			lines(i,j)=0
		Next
	Next
	For i = tempgroupcirclec To circlec
		For j=1 To 12
			circles(i,j)=0
		Next
	Next
	tempint=0
	For i = tempgrouplinec To linec
		tempint=tempint+1
		lines(grouplines(tempint),8)=1
	Next
	tempint=0
	For i = tempgroupcirclec To circlec
		tempint=tempint+1
		circles(groupcircles(tempint),10)=1
	Next
	linec=tempgrouplinec-1
	circlec=tempgroupcirclec-1
End Sub
Sub calctraj()
'dim as double t,d,g,v,vy,vx,theta,dtbc,x,pi,tup,tdn
'	gravity=9.81
'	fbcadupm=1
'	trajiv=0
'	trajtheta=0
'	trajix=0
'	trajiy=0
'	trajiz=0
'
'pi=3.1415926535897932
'g=9.81'gravity
'input "initial velocity";v
'input "theta";theta
'theta=theta*d2r
'input "distance to bottom of cliff";dtbc
'vy=v*sin(theta)
'vx=v*cos(theta)
't=vy/g
'tup=t
'd=t^2*g/2  should be starting y + vy*t - t^2*g/2
'd=d+dtbc this is the distance from top of arc to bottom of cliff
't=sqr(2*d/g)
'tdn=t
'x=vx*(tup+tdn)
'print "point of impact on x axis";x
'print "vertical distance from peak point to bottom of cliff";d 
'sleep
'the final velocity is
'vxf = vx
'vyf = -g*tdn
'vf=sqr(vxf^2 + vyf^2)	
End Sub
Sub plottrajectory()
	'plotting a trajectory is like plotting an ellipse
	Dim As Double trajx
	dim as double t,d,g,v,vy,vx,theta,dtbc,tx,tup,tdn,h,hx
'	gravity=9.81
'	fbcadupm=1
'	trajiv=0
'	trajtheta=0
'	trajix=0
'	trajiy=0
'	trajiz=0
'
'Locate 39,50
'Print trajix,trajiy
'pi=3.1415926535897932
'g=9.81'gravity
g=gravity
'Locate 40,50
'Print "gravity=";g
'input "initial velocity";v
'input "theta";theta
theta=trajtheta*d2r
'Locate 41,50
'Print "theta=";theta

'input "distance to bottom of cliff";dtbc
dtbc=trajiy-modifyy2
'Locate 42,50
'Print "dtbc=";dtbc
vy=trajiv*sin(theta)
'Locate 43,50
'Print "vy=";vy
vx=trajiv*cos(theta)
'Locate 44,50
'Print "vx=";vx
t=vy/g
tup=t
hx=vx*tup+trajix
'Locate 45,50
'Print "t and tup=";t
'd=t^2*g/2
h=trajiy+vy*t - t^2*g/2
If modifyy2>h Then modifyy2=h
Circle (hx,h),5,5
'Locate 46,50
'Print "h=";h
'question, what is x when at max height
'answer is:
'tx=vx*(tup)
'so if mouse is at or above trajiy then
'is mouse to the left or right of tx
'if mouse is to the right of tx then
'procede with calc as if mousey was bottom of cliff
'otherwise
'do the same calc and focus on tdn (time down)
'calc x based on tdn
'as for y, well it's the mouse y (right?)
'as long as mousey is above trajiy
'when mousey is below trajiy, only show x for tdn+tdn


'now for the falling from max height
d=h-modifyy2

'Locate 47,50
'Print "d=";d
t=sqr(2*d/g)
tdn=t
'Locate 48,50
'Print "second t calc and tdn=";t
If modifyy2>trajiy Then
	If mousex<hx Then
		tx=vx*(tup-tdn)
		tx=tx+trajix
		'Locate 55,50
		'Print tdn
	Else
		tx=vx*(tup+tdn)
		tx=tx+trajix
	EndIf
Else
	tx=vx*(tup+tdn)
	tx=tx+trajix
EndIf



	trajx=(trajiv*cos((trajtheta*d2r)))*(((trajiv*sin((trajtheta*d2r)))/gravity)+sqr(2*((((trajiv*sin((trajtheta*d2r)))/gravity)^2*gravity/2)+trajiy-mousey)/gravity))
	trajx=trajx+trajix
	'Circle (trajx,modifyy2),10,10
	Circle (tx,modifyy2),10,10
	'Locate 50,50
	'Print trajx,modifyy2,tx
End Sub
Sub curvesetup()
	Dim As Integer i
	'Dim As Double splinex(splinec+1)
	'Dim As Double spliney(splinec+1)
	Dim As Double splinex(splinec+5)
	Dim As Double spliney(splinec+5)

	'Dim As Double splinetangentangle(splinec)
	Dim As Double splinetangentangle(splinec+4)
	'for some reason splinec was wrong when starting with empty (new) drawing
	'and it was cause the program to crash on the above arrays
	
	Dim As Double splinefx,splinefy
	Dim As Double cmrmax1x,cmrmax1y,cmrmax2x,cmrmax2y
	
	If buttonson(6)=TRUE Then
		For i = 1 To splinec
			splinex(i)=lines(splinepoint(i),1)
			spliney(i)=lines(splinepoint(i),2)
		Next
		splinex(splinec+1)=lines(splinepoint(splinec),4)
		spliney(splinec+1)=lines(splinepoint(splinec),5)
	End If
	If buttonson(83)=TRUE Then
		'force splinec to 3
		splinec=3
		'no need for splinepoint()
		'the translation of that of fourpoints point 1 & 4
		'simply set splinex&y to the translated fourpoints
		splinex(1)=fourpoints(1,1)+(fourpoints(3,1)-fourpoints(2,1))
		spliney(1)=fourpoints(1,2)+(fourpoints(3,2)-fourpoints(2,2))
		splinex(4)=fourpoints(4,1)+(fourpoints(2,1)-fourpoints(3,1))
		spliney(4)=fourpoints(4,2)+(fourpoints(2,2)-fourpoints(3,2))
		'splinex,y(2&3) are not translated but simply the same
		'as fourpoints(2&3)
		splinex(2)=fourpoints(2,1)
		spliney(2)=fourpoints(2,2)
		splinex(3)=fourpoints(3,1)
		spliney(3)=fourpoints(3,2)
	End If
	If buttonson(6)=TRUE Or buttonson(83)=TRUE Then
		For i = 1 To splinec+1
			'Circle (splinex(i),spliney(i)),3,15
			If splinec>1 Then
				Select Case i
					Case 2 To splinec
						xlength=splinex(i+1)-splinex(i-1)
						ylength=spliney(i+1)-spliney(i-1)
						splinetangentangle(i)=atan2(ylength,xlength)*r2d
						If splinetangentangle(i)<0 then splinetangentangle(i)=360+splinetangentangle(i)
						If i>2 Then
							'plot cat_mul_rom
							Dim As BOOLEAN cmrxcrossmaybe
							cmrxcross=FALSE
							cmrxcrossmaybe=FALSE
							Dim As Double cmrt,cmrpx,cmrpy,cmrpxp,cmrpyp,cmrd1,cmrd2,cmrdt,cmrdtp
							Dim As Double cmrxcrossx1,cmrxcrossy1,cmrxcrossx2,cmrxcrossy2,cmrxcrossa
							Dim As Integer cmri,cmri1,cmri2,cmrimax1,cmrimax2
							cmri=1
							cmrt = cmri/100
							cmrpx = calccmr(cmrt,splinex(i-2),splinex(i-1),splinex(i),splinex(i+1))
							cmrpy = calccmr(cmrt,spliney(i-2),spliney(i-1),spliney(i),spliney(i+1))
							cmrd1=calcd(splinex(i-1),spliney(i-1),0,cmrpx,cmrpy,0)
							cmrd2=calcd(splinex(i),spliney(i),0,cmrpx,cmrpy,0)
							cmrdtp=cmrd1+cmrd2
							
							'plot catmul_rom for comparison of ellipse curves
							For cmri=0 To 100
								cmrt=cmri/100
								cmrpx = calccmr(cmrt,splinex(i-2),splinex(i-1),splinex(i),splinex(i+1))
								cmrpy = calccmr(cmrt,spliney(i-2),spliney(i-1),spliney(i),spliney(i+1))
								PSet(cmrpx,cmrpy)
							Next
							
							For cmri = 3 To 97
								cmrt = cmri/100
								cmrpx = calccmr(cmrt,splinex(i-2),splinex(i-1),splinex(i),splinex(i+1))
								cmrpy = calccmr(cmrt,spliney(i-2),spliney(i-1),spliney(i),spliney(i+1))
								cmrd1=calcd(splinex(i-1),spliney(i-1),0,cmrpx,cmrpy,0)
								cmrd2=calcd(splinex(i),spliney(i),0,cmrpx,cmrpy,0)
								cmrdt=cmrd1+cmrd2
								If cmrdt>cmrdtp Then
									if cmrxcrossmaybe=TRUE Then
										If cmrxcross=FALSE Then
											cmrxcrossx2=cmrpx
											cmrxcrossy2=cmrpy
											cmri2=cmri
										EndIf
										cmrxcross=TRUE
									End If
									Select Case cmrxcross
										Case FALSE
											cmrmax1x=cmrpx
											cmrmax1y=cmrpy
											cmrimax1=cmri
										Case TRUE
											cmrmax2x=cmrpx
											cmrmax2y=cmrpy
											cmrimax2=cmri
									End Select
								Else
									If cmrxcross=FALSE Then
										cmrxcrossx1=cmrpxp
										cmrxcrossy1=cmrpyp
										cmri1=cmri
										cmrxcrossmaybe=TRUE
									End If
								EndIf
								cmrdtp=cmrdt
								cmrpxp=cmrpx
								cmrpyp=cmrpy
							Next
							Select Case cmrxcross
								Case FALSE
									curvexy(1,1)=splinex(i-1)
									curvexy(1,2)=spliney(i-1)
									curvexy(1,3)=splinetangentangle(i-1)
									curvexy(2,1)=splinex(i)
									curvexy(2,2)=spliney(i)
									curvexy(2,3)=splinetangentangle(i)
									curvexy(3,1)=splinefx
									curvexy(3,2)=splinefy
									cmrmaxx=cmrmax1x
									cmrmaxy=cmrmax1y
									showcurve()
									'end
								Case TRUE
									'create two curves
									'first curve
									Dim As Double cmrxcrossa1,cmrxcrossa2
									xlength=cmrxcrossx2-cmrxcrossx1
									ylength=cmrxcrossy2-cmrxcrossy1
									cmrxcrossa1=atan2(ylength,xlength)*r2d
									
									cmri=cmri1-(Int(cmri1 - cmrimax1)/2)
									cmrt = cmri/100
									cmrxcrossx1 = calccmr(cmrt,splinex(i-2),splinex(i-1),splinex(i),splinex(i+1))
									cmrxcrossy1 = calccmr(cmrt,spliney(i-2),spliney(i-1),spliney(i),spliney(i+1))
									
									cmri=cmri2+(Int(cmrimax2 - cmri2)/2)
									cmrt = cmri/100
									cmrxcrossx2 = calccmr(cmrt,splinex(i-2),splinex(i-1),splinex(i),splinex(i+1))
									cmrxcrossy2 = calccmr(cmrt,spliney(i-2),spliney(i-1),spliney(i),spliney(i+1))
									
									xlength=cmrxcrossx2-cmrxcrossx1
									ylength=cmrxcrossy2-cmrxcrossy1
									cmrxcrossa2=atan2(ylength,xlength)*r2d
									
									
									'in order to do this right
									'must think about mod 360 issues
									'Select Case cmrxcrossa2
									'	Case Is > cmrxcrossa1
									'		cmrxcrossa=cmrxcrossa2-10
									'	Case Is < cmrxcrossa1
									'		cmrxcrossa=cmrxcrossa2+10
									'End Select
									'instead of trying to get a best (smooth) curve
									'i need to make this user adjustable
									'the idea is to use a single grab handle (a line from
									'cmrxcross to the mouse) in which
									'the user will be rotating about the cmrxcross point
									'increasing and decreasing the angle (cmrxcrossa)
									'so after plotting the two curves the third click is
									'setting the curve down with the adjusted angle
									'perhaps make it possible to simply accept the
									'calculated angle.
									If buttonson(6)=TRUE Then
										cmrxcrossa=cmrxcrossa2
									EndIf
									If buttonson(83)=TRUE Then
										If mouse_clicks=3 Then
											'adjust cmrxcrossa to mouse
											xlength=mousex-cmrxcrossx
											ylength=mousey-cmrxcrossy
											cmrxcrossa=atan2(ylength,xlength)*r2d
											If cmrxcrossa<0 then cmrxcrossa=360+cmrxcrossa
										Else
											cmrxcrossa=cmrxcrossa2
										End If
									End If
									cmrxcrossx=(cmrxcrossx1+cmrxcrossx2)/2
									cmrxcrossy=(cmrxcrossy1+cmrxcrossy2)/2
									
									Dim As Double cmrtangentangle,cmrfx,cmrfy
									xlength=cmrxcrossx-splinex(i-2)
									ylength=cmrxcrossy-spliney(i-2)
									cmrtangentangle=atan2(ylength,xlength)*r2d
									cmrtangentangle+=360
									cmrtangentangle=mymod(cmrtangentangle,360)
									cmrfx=splinex(i)+cos((cmrtangentangle+180)*d2r)*100
									cmrfy=spliney(i)+sin((cmrtangentangle+180)*d2r)*100
									curvexy(1,1)=splinex(i-1)
									curvexy(1,2)=spliney(i-1)
									curvexy(1,3)=splinetangentangle(i-1)
									'curvexy(1,3)=cmrtangentangle
									curvexy(2,1)=cmrxcrossx
									curvexy(2,2)=cmrxcrossy
									curvexy(2,3)=cmrxcrossa
									curvexy(3,1)=cmrfx
									curvexy(3,2)=cmrfy
									cmrmaxx=cmrmax1x
									cmrmaxy=cmrmax1y
									'Circle (cmrmaxx,cmrmaxy),3,3
									showcurve()
									
									'second curve
									xlength=cmrxcrossx-splinex(i+1)
									ylength=cmrxcrossy-spliney(i+1)
									cmrtangentangle=atan2(ylength,xlength)*r2d
									cmrtangentangle+=360
									cmrtangentangle=mymod(cmrtangentangle,360)
									cmrfx=splinex(i)+cos((cmrtangentangle+180)*d2r)*100
									cmrfy=spliney(i)+sin((cmrtangentangle+180)*d2r)*100
									curvexy(1,1)=splinex(i)
									curvexy(1,2)=spliney(i)
									curvexy(1,3)=splinetangentangle(i)
									'curvexy(1,3)=cmrtangentangle
									curvexy(2,1)=cmrxcrossx
									curvexy(2,2)=cmrxcrossy
									curvexy(2,3)=cmrxcrossa
									curvexy(3,1)=cmrfx
									curvexy(3,2)=cmrfy
									cmrmaxx=cmrmax2x
									cmrmaxy=cmrmax2y
									Circle (cmrmaxx,cmrmaxy),3,3
									showcurve()
									Line (cmrmax1x,cmrmax1y)-(cmrmax2x,cmrmax2y),3
							End Select
						EndIf
						'curvesetup()
						splinefx=splinex(i)+cos((splinetangentangle(i)+180)*d2r)*100
						splinefy=spliney(i)+sin((splinetangentangle(i)+180)*d2r)*100
						'Line(splinex(i),spliney(i))-(splinefx,splinefy),15
						'Print c,splinec
				End Select
			End If
		Next
	EndIf
	'Sleep
	'end
End Sub
Sub showcurve()'fillet
	Dim As Double tempcurvexyx, tempcurvexyy, tempcurvexya
	Dim As Double tempfxm, tempfym,temppfxm, temppfym
	Dim As Double tempcurvexyx2, tempcurvexyy2, tempcurvexya2
	Dim As Double tempfxm2, tempfym2, tempfam2
	Dim As Double tempcmrmaxx, tempcmrmaxy
	Dim As BOOLEAN reversed3
	
	tempcmrmaxx=cmrmaxx
	tempcmrmaxy=cmrmaxy
	reversed3=FALSE
	'this is so i can reuse fxm,fym in this code
	fxm=curvexy(2,1)
	fym=curvexy(2,2)
	selangle=curvexy(2,3)
	'now i need a substitue for selline
	'which was derived by splinefx,fy prior to calling this sub routine
	'splinefx=splinex(i)+cos((splinetangentangle(i)+180)*d2r)*100
	'splinefy=spliney(i)+sin((splinetangentangle(i)+180)*d2r)*100
	'then set into variable curvexy(3)
	'modifyx1,y1 Is the  pivot point
'''	'which is the other end of the first line selected
	modifyx1=curvexy(3,1)
	modifyy1=curvexy(3,2)
	modifyx2=curvexy(2,1)+cos(curvexy(2,3)*d2r)*100
	modifyy2=curvexy(2,2)+cos(curvexy(2,3)*d2r)*100
	
	tempcurvexyx2=curvexy(1,1)
	tempcurvexyy2=curvexy(1,2)
	tempcurvexya2=curvexy(1,3)
	tempfxm2=fxm
	tempfym2=fym
	tempfam2=selangle
	calcellipseintersects(curvexy(1,1),curvexy(1,2),curvexy(1,3),fxm,fym,selangle)
	reversed2=FALSE
	If reversed = TRUE Then
		reversed2=TRUE
		'Print "reversed2=true"
		Swap curvexy(1,1),fxm
		Swap curvexy(1,2),fym
		Swap curvexy(1,3),selangle
		Swap modifyx1,modifyx2
		Swap modifyy1,modifyy2
		calcellipseintersects(curvexy(1,1),curvexy(1,2),curvexy(1,3),fxm,fym,selangle)
		If reversed=TRUE Then reversed3=TRUE
	EndIf
	tempcurvexyx=curvexy(1,1)
	tempcurvexyy=curvexy(1,2)
	tempcurvexya=curvexy(1,3)
	z1=0
	'lc=15
	'in the event this requires a rotated elliptical arc then
	'i think a rotate / translate / rotate back is in order
	'modifyx1,y1 Is the  pivot point
	'which is the other end of the first line selected
	
	'rotate curvexy(1-3)
	'rotate pfxm and fxm
	
	angle=360-curvexy(1,3)
	
	x1p=tempcurvexyx-modifyx1
	y1p=tempcurvexyy-modifyy1
	curvexy(1,1)=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
	curvexy(1,2)=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
	
	temppfxm=pfxm
	temppfym=pfym
	x1p=temppfxm-modifyx1
	y1p=temppfym-modifyy1
	pfxm=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
	pfym=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
	
	tempfxm=fxm
	tempfym=fym
	x1p=tempfxm-modifyx1
	y1p=tempfym-modifyy1
	fxm=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
	fym=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
	
	x1p=cmrmaxx-modifyx1
	y1p=cmrmaxy-modifyy1
	cmrmaxx=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
	cmrmaxy=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
	
	eradius=calcd(pfxm,pfym,0,curvexy(1,1),curvexy(1,2),0)
	Select Case (pfym-fym)/(pfym-curvexy(1,2))
		Case -1 To 1
			radius=(fxm-pfxm)/Cos(ASin((pfym-fym)/(pfym-curvexy(1,2))))
		Case Else 'this is cuz the ellipse center can't be found (i think)
			radius=(fxm-pfxm)/Cos(sin((pfym-fym)/(pfym-curvexy(1,2))))
			'Locate 48,50
			'Print "radius error (div by zero error)"
			'Sleep
			'end
	End Select
	If Str(radius)="-1.#IND" Then
	'need to figure out what to do with it (maybe extend one line)
		'end
	else
		eradius=Abs(eradius)
		radius=Abs(radius)
		Dim As Double atfp,atmp,atsp,atcmrmaxp
		xlength=pfxm-curvexy(1,1)
		ylength=(curvexy(1,2)-pfym)*(radius/eradius)
		atfp=atan2(ylength,xlength)*r2d
		If atfp<0 Then atfp=atfp+360
		
		xlength=((curvexy(1,1)+fxm)/2-pfxm)
		ylength=((curvexy(1,2)+fym)/2-pfym)*(radius/eradius)
		atmp=atan2(ylength,xlength)*r2d
		If atmp<0 Then atmp=atmp+360
		
		xlength=(fxm-pfxm)
		ylength=(fym-pfym)*(radius/eradius)
		atsp=atan2(ylength,xlength)*r2d
		If atsp<0 Then atsp=atsp+360
		
		xlength=(cmrmaxx-pfxm)
		ylength=(cmrmaxy-pfym)*(radius/eradius)
		atcmrmaxp=atan2(ylength,xlength)*r2d
		If atcmrmaxp<0 Then atcmrmaxp=atcmrmaxp+360
		
		
		
		'my ellipse is drawn counter clock wise
		'the idea is to define which points are the start and end
		'in order to figure this out
		'just make sure that when drawing the arc
		'that it must start and go counter clock wise thru atmp
		'Print Int(atfp);Int(atmp);Int(atsp);reversed2
		Dim As Integer cai,caic
		For cai=1 To 360
			caic=Int(atfp)+cai
			If caic>360 Then caic=caic-360
			'If caic=Int(atmp) Then
			If caic=Int(atcmrmaxp) Then
				arcstart=atfp
				arcend=atsp
				'If reversed2=FALSE Then Swap arcstart,arcend
				Exit For
			EndIf
			If caic=Int(atsp) Then
				arcstart=atsp
				arcend=atfp
				'If reversed2=FALSE Then Swap arcstart,arcend
				Exit For
			EndIf
		Next
		Select Case shift_key
			Case 0
				
			Case 1,2
				Swap arcstart,arcend
		End Select
		curvexy(1,1)=tempcurvexyx
		curvexy(1,2)=tempcurvexyy
		pfxm=temppfxm
		pfym=temppfym
		fxm=tempfxm
		fym=tempfym
		cmrmaxx=tempcmrmaxx
		cmrmaxy=tempcmrmaxy
		erotation=curvexy(1,3)
		
		If setsplinedown=TRUE Then
			'create ellipse
			x1=pfxm
			y1=pfym
			createellipse()
		Else
			'arcstart=1
			'arcend=359
			plotellipse(pfxm,pfym,z1,radius,lc,arcstart,arcend,eradius,erotation)
		EndIf
		
		If reversed2=TRUE Then
			curvexy(1,1)=tempcurvexyx2
			curvexy(1,2)=tempcurvexyy2
			curvexy(1,3)=tempcurvexya2
			Swap modifyx1,modifyx2
			Swap modifyy1,modifyy2
			fxm=tempfxm2
			fym=tempfym2
			selangle=tempfam2
			reversed2=FALSE
		EndIf
	End if
End Sub
sub duplicate(duplicatemethod As string)
	Dim As Integer i,j,k,l,templc,tempselbutton
	Dim  As Double arclenindegrees,arclengthinradians
	
	tempselbutton=selbutton

	templc=lc

	Dim As BOOLEAN blockstocopy(blockc),blockstoexclude(blockc),rename_block
	Dim As Integer blockrefs(blockc),rename_blocks_c,rename_blocks()
	Dim As Integer templinec,tempcirclec,tempblockc
	
	
	Dim As Integer c,m,n,p
	Dim As BOOLEAN dupe_blockname,rename_rotated_blocks
	'Dim blockstorotate(blockc) As BOOLEAN
	Dim As Integer selected_lines(),selected_lines_c,selected_circles(),selected_circles_c
	Dim As Integer linesinblock(),linesinblockc,circlesinblock(),circlesinblockc
	Dim As Integer templinescount,tempcirclescount
	selected_lines_c=0
	selected_circles_c=0
	
	Dim As Double tempmodifyx1,tempmodifyy1
	tempmodifyx1=modifyx1
	tempmodifyy1=modifyy1
	
	
	
	
	
	
	For i = 1 To blockc
		blockstocopy(i)=FALSE
		blockstoexclude(i)=FALSE
	Next
	templinec=linec
	tempcirclec=circlec
	tempblockc=blockc
	tempint=linec
	for i = 1 to tempint
		if lines(i,8)=1 Then
			selected_lines_c+=1
			Select Case duplicatemethod
				Case "setcopydown"
					x1=lines(i,1)+(modifyx2-modifyx1)
					y1=lines(i,2)+(modifyy2-modifyy1)
					z1=lines(i,3)+(modifyz2-modifyz1)
					x2=lines(i,4)+(modifyx2-modifyx1)
					y2=lines(i,5)+(modifyy2-modifyy1)
					z2=lines(i,6)+(modifyz2-modifyz1)
					lc=lines(i,7)
				Case "setrotatecopydown"
					x1p=lines(i,1)-modifyx1
					y1p=lines(i,2)-modifyy1
					x2p=lines(i,4)-modifyx1
					y2p=lines(i,5)-modifyy1
					x1=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y1=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					x2=x2p*cos(angle*d2r) - y2p*sin(angle*d2r)+modifyx1
					y2=y2p*cos(angle*d2r) + x2p*sin(angle*d2r)+modifyy1
				Case "fliphorizontalcopy"
					x1=modifyx1+(modifyx1-lines(i,1))
					y1=lines(i,2)
					x2=modifyx1+(modifyx1-lines(i,4))
					y2=lines(i,5)
				Case "flipverticalcopy"
					x1=lines(i,1)
					y1=modifyy1+(modifyy1-lines(i,2))
					x2=lines(i,4)
					y2=modifyy1+(modifyy1-lines(i,5))
				'Case ""
				'	
			End Select
			createnewmodlines
			If lines(i,9)>0 Then
				blockstocopy(lines(i,9))=TRUE
				lines(linec,9)=lines(i,9)
				'For j=0 To linesblocklevelc
				'	linesblocklevel(linec,j)=linesblocklevel(i,j)
				'Next
			End If
		Else
			If lines(i,9)>0 Then blockstoexclude(lines(i,9))=TRUE
		End if
	Next
	tempint=circlec
	for i = 1 to tempint
		If circles(i,10)=1 Then
			selected_circles_c+=1
			Select Case duplicatemethod
				Case "setcopydown"
					x1=circles(i,1)+(modifyx2-modifyx1)
					y1=circles(i,2)+(modifyy2-modifyy1)
					z1=circles(i,3)'+(modifyz2-modifyz1) when i do 3d
					radius=circles(i,4)
					lc=circles(i,5)
					arcstart=circles(i,6)
					arcend=circles(i,7)
					aspect=circles(i,8)
					erotation=circles(i,11)
					createnewmodcircles(Int(circles(i,9)))
				Case "setrotatecopydown"
					x1p=circles(i,1)-modifyx1
					y1p=circles(i,2)-modifyy1
					x1=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
					y1=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
					z1=circles(i,3)
					radius=circles(i,4)
					arcstart=circles(i,6)
					arcend=circles(i,7)
					aspect=circles(i,8)
					erotation=circles(i,11)
					Select Case circles(i,9)
						Case 1
							createnewmodcircles(1)
						Case 2
							selcircle=i
							calcarcendpoints
							x1p=arcendpoint1x-modifyx1
							y1p=arcendpoint1y-modifyy1
							x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
							y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
							selarcangle
							arcstart=arcangle*d2r
							x1p=arcendpoint2x-modifyx1
							y1p=arcendpoint2y-modifyy1
							x2=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
							y2=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
							selarcangle
							arcend=arcangle*d2r
							createnewmodcircles(2)
						Case 3,4
							erotation=mymod(erotation+angle,360)
							createnewmodcircles(3)
					End Select
				Case "fliphorizontalcopy"
					x1=circles(i,1)
					y1=circles(i,2)
					z1=circles(i,3)
					radius=circles(i,4)
					arcstart=circles(i,6)
					arcend=circles(i,7)
					aspect=circles(i,8)
					erotation=circles(i,11)
					x1=modifyx1+(modifyx1-x1)
					Select Case circles(i,9)
						Case 1
							createnewmodcircles(1)
						Case 2
							If arcstart>arcend Then
								arclengthinradians= (PI*2)-arcstart+arcend
							Else
								arclengthinradians=arcend-arcstart
							EndIf
							selcircle=i
							calcarcendpoints
							x1p=modifyx1+(modifyx1-arcendpoint2x)
							x2=x1p
							y2=arcendpoint2y
							selarcangle
							arcstart=arcangle*d2r
							arcend=arcstart+arclengthinradians
							aspect=0
							createnewmodcircles(2)
						Case 3,4
							tempdouble=360-arcstart-arcend
							arcstart=arcstart+tempdouble+180
							arcend=arcend+tempdouble+180
							arcstart=mymod(arcstart,360)
							arcend=mymod(arcend,360)
							If arcstart=arcend Then
								arcstart=0
								arcend=360
							EndIf
							erotation=360-erotation
							erotation=mymod(erotation,360)
							createnewmodcircles(3)
					End Select
				Case "flipverticalcopy"
					x1=circles(i,1)
					y1=circles(i,2)
					z1=circles(i,3)
					radius=circles(i,4)
					arcstart=circles(i,6)
					arcend=circles(i,7)
					aspect=circles(i,8)
					erotation=circles(i,11)
					y1=modifyy1+(modifyy1-y1)
					Select Case circles(i,9)
						Case 1
							createnewmodcircles(1)
						Case 2
							If arcstart>arcend Then
								arclengthinradians= (PI*2)-arcstart+arcend
							Else
								arclengthinradians=arcend-arcstart
							EndIf
							selcircle=i
							calcarcendpoints
							y1p=modifyy1+(modifyy1-arcendpoint2y)'use this for vertical
							x2=arcendpoint2x
							y2=y1p
							selarcangle
							arcstart=arcangle*d2r
							arcend=arcstart+arclengthinradians
							createnewmodcircles(2)
						Case 3,4
							tempdouble=360-arcstart-arcend
							arcstart=arcstart+tempdouble
							arcend=arcend+tempdouble
							arcstart=mymod(arcstart,360)
							arcend=mymod(arcend,360)
							If arcstart=arcend Then
								arcstart=0
								arcend=360
							EndIf
							erotation=360-erotation
							erotation=mymod(erotation,360)
							createnewmodcircles(3)
					End Select
				'Case ""
				'	
			End Select
			If circles(i,12)>0 Then
				blockstocopy(circles(i,12))=TRUE
				circles(circlec,12)=circles(i,12)
				'For j=0 To circlesblocklevelc
				'	circlesblocklevel(circlec,j)=circlesblocklevel(i,j)
				'Next
			End If
		Else
			If circles(i,12)>0 Then blockstoexclude(circles(i,12))=TRUE
		EndIf
	Next
	'if the new entities created belong to a block but excluded then remove their block status
	For i = templinec+1 To linec
		If blockstoexclude(lines(i,9))=TRUE Then lines(i,9)=0
	Next
	For i = tempcirclec+1 To circlec
		If blockstoexclude(circles(i,12))=TRUE Then circles(i,12)=0
	Next
	For i = 1 To blockc
		'If blockstoexclude(i)=FALSE Then blockstocopy(i)=TRUE Else blockstocopy(i)=FALSE
		If blockstoexclude(i)=TRUE Then blockstocopy(i)=FALSE
	Next
	Dim blockstocopyc As Integer
	blockstocopyc=0
	For i = 1 To blockc
		If blockstocopy(i)=TRUE Then blockstocopyc=blockstocopyc+1
	Next
	blockc=blockc+blockstocopyc
	ReDim Preserve blocknames(blockc)
	ReDim Preserve blockoffsets(blockc,7)
	ReDim Preserve blockstatus(blockc)
	
	If blocklevelc>0 Then
		ReDim tempintarray(tempblockc,blocklevelc+1)
		For i = 1 To tempblockc
			For j=1 To blocklevelc+1
				tempintarray(i,j)=blocklevel(i,j)
			Next
		Next
		ReDim blocklevel(blockc,blocklevelc+1)
		For i = 1 To tempblockc
			For j=1 To blocklevelc+1
				blocklevel(i,j)=tempintarray(i,j)
			Next
		Next
	End If
	
	blockstocopyc=0
	ReDim Preserve rename_blocks(rename_blocks_c)
	For i = 1 To tempblockc'blockc
		If blockstocopy(i)=TRUE Then
			blockstocopyc=blockstocopyc+1
			blockrefs(i)=tempblockc+blockstocopyc
			blocknames(tempblockc+blockstocopyc)=blocknames(i)
			Select Case duplicatemethod'for block offsets info
				'(blockc,7) 1=insertionx,2=insertiony,3=insertionz,4=scalex,5=scaley,6=scalez,7=rotation
				Case "setcopydown"
					blockoffsets(tempblockc+blockstocopyc,1)=blockoffsets(i,1)+(modifyx2-modifyx1)
					blockoffsets(tempblockc+blockstocopyc,2)=blockoffsets(i,2)+(modifyy2-modifyy1)
					blockoffsets(tempblockc+blockstocopyc,3)=blockoffsets(i,3)+(0-0)
					blockoffsets(tempblockc+blockstocopyc,4)=blockoffsets(i,4)
					blockoffsets(tempblockc+blockstocopyc,5)=blockoffsets(i,5)
					blockoffsets(tempblockc+blockstocopyc,6)=blockoffsets(i,6)
					blockoffsets(tempblockc+blockstocopyc,7)=blockoffsets(i,7)
				Case "setrotatecopydown"
					'if the block has been scaled previous to rotate then create new block using block name plus transformation and next transfomation number.
					'see code from setrotatedown
					Select Case blockoffsets(i,4)
						Case 0,1
							Select Case blockoffsets(i,5)
								Case 0,1
									rename_block=FALSE
								Case Else
									rename_block=TRUE
							End Select
						Case Else
							rename_block=TRUE
					End Select
					If rename_block=FALSE Then
						x1p=blockoffsets(i,1)-modifyx1
						y1p=blockoffsets(i,2)-modifyy1
						blockoffsets(tempblockc+blockstocopyc,1)=x1p*cos(angle*d2r) - y1p*sin(angle*d2r)+modifyx1
						blockoffsets(tempblockc+blockstocopyc,2)=y1p*cos(angle*d2r) + x1p*sin(angle*d2r)+modifyy1
						blockoffsets(tempblockc+blockstocopyc,3)=blockoffsets(i,3)+(0-0)
						blockoffsets(tempblockc+blockstocopyc,4)=blockoffsets(i,4)
						blockoffsets(tempblockc+blockstocopyc,5)=blockoffsets(i,5)
						blockoffsets(tempblockc+blockstocopyc,6)=blockoffsets(i,6)
						blockoffsets(tempblockc+blockstocopyc,7)=mymod(blockoffsets(i,7)+angle,360)
					Else
						rename_rotated_blocks=TRUE
						rename_blocks_c+=1
						ReDim Preserve rename_blocks(rename_blocks_c)
						rename_blocks(rename_blocks_c)=tempblockc+blockstocopyc
					EndIf
				Case "fliphorizontalcopy"
					blockoffsets(tempblockc+blockstocopyc,1)=modifyx1+(modifyx1-blockoffsets(i,1))'(modifyx2-modifyx1)
					blockoffsets(tempblockc+blockstocopyc,2)=blockoffsets(i,2)
					blockoffsets(tempblockc+blockstocopyc,3)=blockoffsets(i,3)
					blockoffsets(tempblockc+blockstocopyc,4)=blockoffsets(i,4)*-1
					blockoffsets(tempblockc+blockstocopyc,5)=blockoffsets(i,5)
					blockoffsets(tempblockc+blockstocopyc,6)=blockoffsets(i,6)
					blockoffsets(tempblockc+blockstocopyc,7)=blockoffsets(i,7)
				Case "flipverticalcopy"
					blockoffsets(tempblockc+blockstocopyc,1)=blockoffsets(i,1)
					blockoffsets(tempblockc+blockstocopyc,2)=modifyy1+(modifyy1-blockoffsets(i,2))
					blockoffsets(tempblockc+blockstocopyc,3)=blockoffsets(i,3)
					blockoffsets(tempblockc+blockstocopyc,4)=blockoffsets(i,4)
					blockoffsets(tempblockc+blockstocopyc,5)=blockoffsets(i,5)*-1
					blockoffsets(tempblockc+blockstocopyc,6)=blockoffsets(i,6)
					blockoffsets(tempblockc+blockstocopyc,7)=blockoffsets(i,7)
				'Case ""
				'	
			End Select
			blockstatus(tempblockc+blockstocopyc)=blockstatus(i)
			If blocklevelc>0 Then
				'blocklevel(tempblockc+blockstocopyc,1)=blocklevel(i,1)
				If blocklevel(i,1)>0 Then'if is a child (nested block) with parent info then
					'is the parent referenced included in the list of blocks to copy?
					For j=1 To blocklevel(i,1)
						If blockstocopy(blocklevel(i,j+1))=FALSE Then
							blockstocopy(blocklevel(i,j+1))=TRUE
							blockc=blockc+1
							ReDim Preserve blocknames(blockc)
							ReDim Preserve blockoffsets(blockc,7)
							ReDim Preserve blockstatus(blockc)
							ReDim blocklevel(blockc,blocklevelc+1)
							For k = 1 To tempblockc
								For l=1 To blocklevelc+1
									blocklevel(k,l)=tempintarray(k,l)
								Next
							Next
						EndIf
					Next
				End If
			EndIf
			For j=templinec+1 To linec
				If lines(j,9)=i Then lines(j,9)=tempblockc+blockstocopyc
			Next
			For j=tempcirclec+1 To circlec
				If circles(j,12)=i Then circles(j,12)=tempblockc+blockstocopyc
			Next
		EndIf
	Next
	For i = 1 To tempblockc
		If blocklevelc>0 Then
			If blockrefs(i)<>0 Then
				If blocklevel(i,1)>0 Then
					blocklevel(blockrefs(i),1)=blocklevel(i,1)
					For j = 2 To blocklevelc+1
						blocklevel(blockrefs(i),j)=blockrefs(blocklevel(i,j))
					Next
				EndIf
			EndIf
		EndIf
	Next
	If templinec<>linec Then
		Select Case linec-templinec
			Case 1
				theboxbelow(Str(linec-templinec)+" New line created - Line count="+Str(linec-preloadedlinec))
			Case Else
				theboxbelow(Str(linec-templinec)+" New lines created - Line count="+Str(linec-preloadedlinec))
		End Select
	EndIf
	If tempcirclec<>circlec Then
		Select Case circlec-tempcirclec
			Case 1
				theboxbelow(Str(circlec-tempcirclec)+" New circle created - Circle count="+Str(circlec-preloadedcirclec))
			Case Else
				theboxbelow(Str(circlec-tempcirclec)+" New circles created - Circle count="+Str(circlec-preloadedcirclec))
		End Select
	EndIf
	lc=templc
	
	If rename_rotated_blocks=TRUE Then
		templinescount=linec
		tempcirclescount=circlec
		ReDim selected_lines(selected_lines_c)
		ReDim selected_circles(selected_circles_c)
		c=0
		for i = 1 to linec
			if lines(i,8)=1 Then
				c+=1
				selected_lines(c)=i
			EndIf
		Next
		c=0
		For i = 1 To circlec	
			If circles(i,10)=1 Then
				c+=1
				selected_circles(c)=i
			EndIf
		Next
		degroup
		For j = 1 To rename_blocks_c
			i=rename_blocks(j)
			'rename block
			theboxbelow("block "+ blocknames(i) +" is being renamed to "+ blocknames(i) +"-transformation.1 due to muilti transformation")
			'create new block with blockname + transfomation number
			c=0
			For k = 1 to linec
				if lines(k,9)=i Then
					c+=1
					lines(k,8)=1
				EndIf
			Next
			linesinblockc=c
			ReDim linesinblock(linesinblockc)
			c=0
			for k = 1 to linec
				if lines(k,9)=i Then
					c+=1
					linesinblock(c)=k
				EndIf
			Next
			c=0
			For k = 1 To circlec	
				If circles(k,12)=i Then
					c+=1
					circles(k,10)=1
				EndIf
			Next
			circlesinblockc=c
			ReDim circlesinblock(circlesinblockc)
			c=0
			For k = 1 To circlec	
				If circles(k,12)=i Then
					c+=1
					circlesinblock(c)=k
				EndIf
			Next
			explode_block
			For k=1 To linesinblockc
				lines(linesinblock(k),8)=1
			Next
			For k=1 To circlesinblockc
				circles(circlesinblock(k),10)=1
			Next
			groupexists=TRUE
			If InStr(blocknames(i),"_trans_ver.")<>0 Then
				c=InStr(blocknames(i),"_trans_ver.")
				m=0
				Do
					m+=1
					blockname=Left(blocknames(i),c-1)+"_trans_ver."+LTrim(Str(ValInt(Mid(blocknames(i),c+11))+m))
					dupe_blockname=FALSE
					For n=1 To blockc	
						If blockname=blocknames(n) Then
							dupe_blockname=TRUE
							Exit For
						EndIf
					Next
					If dupe_blockname=FALSE Then Exit Do
				Loop
			Else
				blockname=blocknames(i)+"_trans_ver.0"
				c=InStr(blockname,"_trans_ver.")
				m=0
				Do
					m+=1
					blockname=Left(blockname,c-1)+"_trans_ver."+LTrim(Str(m))
					dupe_blockname=FALSE
					For n=1 To blockc
						theboxbelow(Str(n)+" checking for dupe block name "+blockname+" against "+blocknames(n))
						If blockname=blocknames(n) Then
							dupe_blockname=TRUE
							Exit For
						EndIf
					Next
					If dupe_blockname=FALSE Then Exit Do
				Loop
			EndIf
			selbutton=111'create block
			buttonmanager
			degroup
		Next
		groupexists=TRUE
		For i=1 To selected_lines_c
			lines(selected_lines(i),8)=1
		Next
		For i=1 To selected_circles_c
			circles(selected_circles(i),10)=1
		Next
		selbutton=tempselbutton
	EndIf	
	modifyx1=tempmodifyx1
	modifyy1=tempmodifyy1

	
	inview
	redraw
	'initlinedraw
end sub
Sub countactiveobjects()
	Dim As Integer i
	activelinesc=0
	activecirclesc=0
	activeblocksc=0
	For i=1 To linec
		If lines(i,8)=0 Then activelinesc=activelinesc+1
	Next
	For i=1 To circlec
		If circles(i,10)=0 Then activecirclesc=activecirclesc+1
	Next
	For i = 1 To blockc
		If blockstatus(i)=-1 Then activeblocksc=activeblocksc+1
	Next
End Sub
Sub textviewer(texttoview As String)
	'this will display text in users default browser
	'the texttoview needs to be converted to treat crlf in txt as html <br>
	'create a temporary file called whatever.htm from texttoview string
	'then shell "start whatever.htm"
End Sub
Sub newdrawing()
	'there must be a bunch of global variales that need to be reset
	drawareax1=0
	drawareax2=579
	drawareay1=0
	drawareay2=579
	wx1=0
	wy1=0
	wx2=drawareax2-drawareax1
	wy2=drawareay2-drawareay1


	linememstep=1000
	circlememstep=1000
	ReDim Preserve lines(preloadedlinec,9)
	ReDim Preserve lines(linememstep,9)
	redim inviewlines(linememstep)
	redim inviewfilterlines(linememstep)
	ReDim Preserve circles(preloadedcirclec,12)
	ReDim Preserve circles(circlememstep,12)
	redim inviewcircles(circlememstep)
	redim inviewfiltercircles(circlememstep)

	linec=preloadedlinec
	circlec=preloadedcirclec

	blockc=0


End Sub
Sub show_polygon(pnos As Integer)
	Dim As Integer i,j,k
	Dim As Double pptx(pnos),ppty(pnos),psa,psai,papothem,pradius,psl,psangle,psl_to_papothem_ratio,psmpx,psmpy,pcenterx,pcentery,psxlength,psylength
	psa=360/pnos
	xlength=x2-x1
	ylength=y2-y1
	fixangle
	If alt_key=1 Or alt_key=2 Then
		If shift_key=1 Or shift_key=2 Then
			For i= 1 To pnos
				psai=psa*(i-1)+angle
				pptx(i)=(x1+(x2-x1)/2)+cos(psai*d2r)*length/2
				ppty(i)=(y1+(y2-y1)/2)+Sin(psai*d2r)*length/2
			Next
		Else
			For i= 1 To pnos
				psai=psa*(i-1)+angle
				pptx(i)=x1+cos(psai*d2r)*length
				ppty(i)=y1+Sin(psai*d2r)*length
			Next
		EndIf
	Else
		For i= 1 To pnos
			psai=psa*(i-1)+angle
			pptx(i)=x1+cos(psai*d2r)*length
			ppty(i)=y1+Sin(psai*d2r)*length
		Next
		psl=sqr((pptx(1)-pptx(2))^2 + (ppty(1)-ppty(2))^2)
		papothem=Sqr(Abs(length^2-(psl/2)^2))
		psl_to_papothem_ratio=papothem/psl
		papothem=length*psl_to_papothem_ratio
		psmpx=x1+(x2-x1)/2
		psmpy=y1+(y2-y1)/2
		pcenterx=psmpx+cos((angle+90)*d2r)*papothem
		pcentery=psmpy+Sin((angle+90)*d2r)*papothem
		pradius=sqr((x1-pcenterx)^2 + (y1-pcentery)^2)
		psxlength=x1-pcenterx
		psylength=y1-pcentery
		psangle=atan2(psylength,psxlength)*r2d
		If psangle<0 then psangle=360+psangle
		For i= 1 To pnos
			psai=psa*(i-1)+psangle
			pptx(i)=pcenterx+cos(psai*d2r)*pradius
			ppty(i)=pcentery+Sin(psai*d2r)*pradius
		Next
	EndIf
	For i = 1 To pnos-1
		Line(pptx(i),ppty(i))-(pptx(i+1),ppty(i+1))
	Next
	Line(pptx(pnos),ppty(pnos))-(pptx(1),ppty(1))

End Sub
Sub set_polygon_down(pnos As Integer)
	Dim As Integer i,j,k
	Dim As Double pptx(pnos),ppty(pnos),psa,psai,papothem,pradius,psl,psangle,psl_to_papothem_ratio,psmpx,psmpy,pcenterx,pcentery,psxlength,psylength
	psa=360/pnos
	xlength=x2-x1
	ylength=y2-y1
	fixangle
	If alt_key=1 Or alt_key=2 Then
		If shift_key=1 Or shift_key=2 Then
			For i= 1 To pnos
				psai=psa*(i-1)+angle
				pptx(i)=(x1+(x2-x1)/2)+cos(psai*d2r)*length/2
				ppty(i)=(y1+(y2-y1)/2)+Sin(psai*d2r)*length/2
			Next
		Else
			For i= 1 To pnos
				psai=psa*(i-1)+angle
				pptx(i)=x1+cos(psai*d2r)*length
				ppty(i)=y1+Sin(psai*d2r)*length
			Next
		EndIf
	Else
		For i= 1 To pnos
			psai=psa*(i-1)+angle
			pptx(i)=x1+cos(psai*d2r)*length
			ppty(i)=y1+Sin(psai*d2r)*length
		Next
		psl=sqr((pptx(1)-pptx(2))^2 + (ppty(1)-ppty(2))^2)
		papothem=Sqr(Abs(length^2-(psl/2)^2))
		psl_to_papothem_ratio=papothem/psl
		papothem=length*psl_to_papothem_ratio
		psmpx=x1+(x2-x1)/2
		psmpy=y1+(y2-y1)/2
		pcenterx=psmpx+cos((angle+90)*d2r)*papothem
		pcentery=psmpy+Sin((angle+90)*d2r)*papothem
		pradius=sqr((x1-pcenterx)^2 + (y1-pcentery)^2)
		psxlength=x1-pcenterx
		psylength=y1-pcentery
		psangle=atan2(psylength,psxlength)*r2d
		If psangle<0 then psangle=360+psangle
		For i= 1 To pnos
			psai=psa*(i-1)+psangle
			pptx(i)=pcenterx+cos(psai*d2r)*pradius
			ppty(i)=pcentery+Sin(psai*d2r)*pradius
		Next
	EndIf
	For i = 1 To pnos-1
		memmanageline
		lines(linec,1)=pptx(i)
		lines(linec,2)=ppty(i)
		lines(linec,4)=pptx(i+1)
		lines(linec,5)=ppty(i+1)
		lines(linec,7)=lc
	Next
	memmanageline
	lines(linec,1)=pptx(pnos)
	lines(linec,2)=ppty(pnos)
	lines(linec,4)=pptx(1)
	lines(linec,5)=ppty(1)
	lines(linec,7)=lc
	'memmanagecircle
	'circles(circlec,1)=pcenterx
	'circles(circlec,2)=pcentery
	'circles(circlec,4)=pradius
	'circles(circlec,5)=lc
	'circles(circlec,9)=1
	drawing=false
	initlinedraw
	escapeme
End Sub
Sub draw_right_triangle()
	Dim As Double rtptx,rtpty,rtangle
	Select Case mouse_clicks
		Case 1
			xlength=x2-x1
			ylength=y2-y1
			fixangle
			rtangle=angle+90
			rtptx=x1+cos(rtangle*d2r)*length
			rtpty=y1+Sin(rtangle*d2r)*length
			right_triangle_x1=x1
			right_triangle_y1=y1
			right_triangle_x2=x2
			right_triangle_y2=y2
			right_triangle_angle=angle
			Line(right_triangle_x1,right_triangle_y1)-(right_triangle_x2,right_triangle_y2)
			Line(right_triangle_x1,right_triangle_y1)-(rtptx,rtpty)
			Line(right_triangle_x2,right_triangle_y2)-(rtptx,rtpty)
		Case 2
			If ortho=TRUE Then
				selbutton=21
				mousexp=mousex-1
				tempmousex=mousexp-1
				turnbuttonoff
				Exit Sub
			EndIf
			If drawatangle=TRUE Then
				selbutton=22
				mousexp=mousex-1
				tempmousex=mousexp-1
				drawatangle=FALSE
				forcex=false
				forcey=FALSE
				turnbuttonoff
				Exit Sub
			EndIf
			rtangle=right_triangle_angle+90
			rtptx=x1+cos(rtangle*d2r)*length
			rtpty=y1+Sin(rtangle*d2r)*length
			Line(right_triangle_x1,right_triangle_y1)-(right_triangle_x2,right_triangle_y2)
			Line(right_triangle_x1,right_triangle_y1)-(rtptx,rtpty)
			Line(right_triangle_x2,right_triangle_y2)-(rtptx,rtpty)
		Case 3
			rtangle=right_triangle_angle+90
			rtptx=x1+cos(rtangle*d2r)*length
			rtpty=y1+Sin(rtangle*d2r)*length
			memmanageline
			lines(linec,1)=right_triangle_x1
			lines(linec,2)=right_triangle_y1
			lines(linec,4)=right_triangle_x2
			lines(linec,5)=right_triangle_y2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=right_triangle_x1
			lines(linec,2)=right_triangle_y1
			lines(linec,4)=rtptx
			lines(linec,5)=rtpty
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=right_triangle_x2
			lines(linec,2)=right_triangle_y2
			lines(linec,4)=rtptx
			lines(linec,5)=rtpty
			lines(linec,7)=lc
			drawing=false
			initlinedraw
			escapeme
	End Select
End Sub
Sub draw_rectangle()
	Dim As Double rectangleptx1,rectanglepty1,rectangleptx2,rectanglepty2,rectangleangle
	Select Case mouse_clicks
		Case 1
			xlength=x2-x1
			ylength=y2-y1
			fixangle
			rectangleangle=angle+90
			rectangle_x1=x1
			rectangle_y1=y1
			rectangle_x2=x2
			rectangle_y2=y2
			rectangle_angle=angle
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=x2+cos(rectangleangle*d2r)*length
			rectanglepty2=y2+Sin(rectangleangle*d2r)*length
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 2
			If ortho=TRUE Then
				selbutton=21
				mousexp=mousex-1
				tempmousex=mousexp-1
				turnbuttonoff
				Exit Sub
			EndIf
			If drawatangle=TRUE Then
				selbutton=22
				mousexp=mousex-1
				tempmousex=mousexp-1
				drawatangle=FALSE
				forcex=false
				forcey=FALSE
				turnbuttonoff
				Exit Sub
			EndIf
			length = sqr((rectangle_x2-x2)^2 + (rectangle_y2-y2)^2)
			rectangleangle=rectangle_angle+90
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 3
			length = sqr((rectangle_x2-x2)^2 + (rectangle_y2-y2)^2)
			rectangleangle=rectangle_angle+90
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			memmanageline
			lines(linec,1)=rectangle_x1
			lines(linec,2)=rectangle_y1
			lines(linec,4)=rectangle_x2
			lines(linec,5)=rectangle_y2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangle_x1
			lines(linec,2)=rectangle_y1
			lines(linec,4)=rectangleptx1
			lines(linec,5)=rectanglepty1
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangle_x2
			lines(linec,2)=rectangle_y2
			lines(linec,4)=rectangleptx2
			lines(linec,5)=rectanglepty2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangleptx1
			lines(linec,2)=rectanglepty1
			lines(linec,4)=rectangleptx2
			lines(linec,5)=rectanglepty2
			lines(linec,7)=lc
			drawing=false
			initlinedraw
			escapeme
	End Select
End Sub
Sub draw_parallelogram()
	Dim As Double rectangleptx1,rectanglepty1,rectangleptx2,rectanglepty2,rectangleangle
	Select Case mouse_clicks
		Case 1
			xlength=x2-x1
			ylength=y2-y1
			fixangle
			rectangleangle=angle+90
			rectangle_x1=x1
			rectangle_y1=y1
			rectangle_x2=x2
			rectangle_y2=y2
			rectangle_angle=angle
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=x2+cos(rectangleangle*d2r)*length
			rectanglepty2=y2+Sin(rectangleangle*d2r)*length
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 2
			If ortho=TRUE Then
				selbutton=21
				mousexp=mousex-1
				tempmousex=mousexp-1
				turnbuttonoff
				Exit Sub
			EndIf
			If drawatangle=TRUE Then
				selbutton=22
				mousexp=mousex-1
				tempmousex=mousexp-1
				drawatangle=FALSE
				forcex=false
				forcey=FALSE
				turnbuttonoff
				Exit Sub
			EndIf
			length = sqr((rectangle_x2-x2)^2 + (rectangle_y2-y2)^2)
			rectangleangle=rectangle_angle+90
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			rectangle_x3=rectangleptx2
			rectangle_y3=rectanglepty2
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 3
			length = sqr((rectangle_x2-rectangle_x3)^2 + (rectangle_y2-rectangle_y3)^2)
			rectangleangle=rectangle_angle+90
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			length = sqr((x2-rectangle_x3)^2 + (y2-rectangle_y3)^2)
			rectangleangle=rectangle_angle
			If sqr((x2-rectangleptx1)^2 + (y2-rectanglepty1)^2)<sqr((rectangleptx1-rectangleptx2)^2 + (rectanglepty1-rectanglepty2)^2) Then rectangleangle+=180
			rectangleptx1=rectangleptx1+cos(rectangleangle*d2r)*length
			rectanglepty1=rectanglepty1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangleptx2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectanglepty2+Sin(rectangleangle*d2r)*length
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 4
			length = sqr((rectangle_x2-rectangle_x3)^2 + (rectangle_y2-rectangle_y3)^2)
			rectangleangle=rectangle_angle+90
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			length = sqr((x2-rectangle_x3)^2 + (y2-rectangle_y3)^2)
			rectangleangle=rectangle_angle
			If sqr((x2-rectangleptx1)^2 + (y2-rectanglepty1)^2)<sqr((rectangleptx1-rectangleptx2)^2 + (rectanglepty1-rectanglepty2)^2) Then rectangleangle+=180
			rectangleptx1=rectangleptx1+cos(rectangleangle*d2r)*length
			rectanglepty1=rectanglepty1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangleptx2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectanglepty2+Sin(rectangleangle*d2r)*length
			memmanageline
			lines(linec,1)=rectangle_x1
			lines(linec,2)=rectangle_y1
			lines(linec,4)=rectangle_x2
			lines(linec,5)=rectangle_y2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangle_x1
			lines(linec,2)=rectangle_y1
			lines(linec,4)=rectangleptx1
			lines(linec,5)=rectanglepty1
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangle_x2
			lines(linec,2)=rectangle_y2
			lines(linec,4)=rectangleptx2
			lines(linec,5)=rectanglepty2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangleptx1
			lines(linec,2)=rectanglepty1
			lines(linec,4)=rectangleptx2
			lines(linec,5)=rectanglepty2
			lines(linec,7)=lc
			drawing=false
			initlinedraw
			escapeme
	End Select
End Sub
Sub draw_rhombus()
	Dim As Double rectangleptx1,rectanglepty1,rectangleptx2,rectanglepty2,rectangleangle
	Select Case mouse_clicks
		Case 1
			xlength=x2-x1
			ylength=y2-y1
			fixangle
			rectangleangle=angle+90
			rectangle_x1=x1
			rectangle_y1=y1
			rectangle_x2=x2
			rectangle_y2=y2
			rectangle_angle=angle
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=x2+cos(rectangleangle*d2r)*length
			rectanglepty2=y2+Sin(rectangleangle*d2r)*length
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 2
			If ortho=TRUE Then
				selbutton=21
				mousexp=mousex-1
				tempmousex=mousexp-1
				turnbuttonoff
				Exit Sub
			EndIf
			If drawatangle=TRUE Then
				selbutton=22
				mousexp=mousex-1
				tempmousex=mousexp-1
				drawatangle=FALSE
				forcex=false
				forcey=FALSE
				turnbuttonoff
				Exit Sub
			EndIf
			length = sqr((rectangle_x1-rectangle_x2)^2 + (rectangle_y1-rectangle_y2)^2)
			rectangleangle=angle
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			Line(rectangle_x1,rectangle_y1)-(rectangle_x2,rectangle_y2)
			Line(rectangle_x1,rectangle_y1)-(rectangleptx1,rectanglepty1)
			Line(rectangle_x2,rectangle_y2)-(rectangleptx2,rectanglepty2)
			Line(rectangleptx1,rectanglepty1)-(rectangleptx2,rectanglepty2)
		Case 3
			length = sqr((rectangle_x1-rectangle_x2)^2 + (rectangle_y1-rectangle_y2)^2)
			rectangleangle=angle
			rectangleptx1=x1+cos(rectangleangle*d2r)*length
			rectanglepty1=y1+Sin(rectangleangle*d2r)*length
			rectangleptx2=rectangle_x2+cos(rectangleangle*d2r)*length
			rectanglepty2=rectangle_y2+Sin(rectangleangle*d2r)*length
			memmanageline
			lines(linec,1)=rectangle_x1
			lines(linec,2)=rectangle_y1
			lines(linec,4)=rectangle_x2
			lines(linec,5)=rectangle_y2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangle_x1
			lines(linec,2)=rectangle_y1
			lines(linec,4)=rectangleptx1
			lines(linec,5)=rectanglepty1
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangle_x2
			lines(linec,2)=rectangle_y2
			lines(linec,4)=rectangleptx2
			lines(linec,5)=rectanglepty2
			lines(linec,7)=lc
			memmanageline
			lines(linec,1)=rectangleptx1
			lines(linec,2)=rectanglepty1
			lines(linec,4)=rectangleptx2
			lines(linec,5)=rectanglepty2
			lines(linec,7)=lc
			drawing=false
			initlinedraw
			escapeme
	End Select
End Sub
Sub loadpic
	Exit Sub 
DIM garray(4 * (600 * 240) + 4) AS Byte
BLOAD "2.bmp", @garray(0)

Dim As Integer w,h,x,y,c,cx,cy,tlx,tly
tlx=251
tly=31


PUT (tlx,tly),garray(0)
'sleep
w=219
h=219
for x = tlx to w+tlx
    for y=tly to h+tly
        if point(x,y)=0 then
            pset(x,y),250
        else
            pset(x,y),0
        end if
    next
next
for x = tlx to w+tlx
    for y=tly to h+tly
        if point(x,y)=250 then
            c=0
            for cx=x-1 to x+1
                for cy=y-1 to y+1
                    if point(cx,cy)=0 then
                        pset(x,y),125000
                        c=1
                        exit for
                    end if
                next
                if c=1 then exit for
            next
        end if
    next
next
for x = tlx to w+tlx
    for y=tly to h+tly
        if point(x,y)=250 then
            pset(x,y),0
        end if
    next
next
'outline complete
'for x = tlx to w+tlx
'    for y=tly to h+tly
'        if point(x,y)<>0 then
'            c=0
'            for cx=x-1 to x+1
'                for cy=y-1 to y+1
'                    if cx=x and cy=y then
'                        
'                    else
'                        if point(cx,cy)<>0 then c=c+1
'                    end if
'                next
'            next
'            if c=2 then
'                if point(x-1,y)<>0 and point(x+1,y)<>0 then
'                    'horizontal line
'                else
'                    if point(x,y-1)<>0 and point(x,y+1)<>0 then
'                        'vertical line
'                    else
'                        'corner
'                        pset(x,y),250
'                    end if
'                end if
'                
'            end if
'        end if
'    next
'next
Sleep
End Sub
Sub buttonmanager()
	if buttonson(selbutton)=true then
		turnbuttonoff2
	else
		turnbuttonon
	end If
End Sub
Function inputbox(datatype As String,prompt As String,initvalue As String) As String
	Dim ibstring As String
	'datatype will either be "double" or "string" 
	'"string" is used for entering text into the drawing
	'"double" is used for a number
	Select Case datatype
		'Case "string"
		'	'call same routine for entering text
		Case "string","double"
			'show dialog box to enter new preset angle
			Dim As GtkWidget PTR dialog,content
			Dim As GtkWidget PTR dg'dialog grid
			Dim As GtkWidget PTR dge1'dialog grid entry(n)
			
			Dim As Integer response
			dialog = gtk_dialog_new()
			gtk_window_set_modal( GTK_WINDOW( dialog ), TRUE )
			gtk_window_set_transient_for(GTK_WINDOW(dialog),GTK_WINDOW(win))
			gtk_window_set_title(GTK_WINDOW(dialog), prompt)
			'gtk_window_set_resizable (GTK_WINDOW(dialog),FALSE)
			gtk_window_set_default_size(GTK_WINDOW(dialog), 200, 100)
			content = gtk_dialog_get_content_area(GTK_DIALOG(dialog))
			
			dg = gtk_grid_new()
			dge1 = gtk_entry_new()
			gtk_entry_set_width_chars (dge1,20)
			gtk_entry_set_text(GTK_ENTRY(dge1),initvalue)
			gtk_grid_attach (GTK_GRID(dg), dge1, 1, 0, 1, 1)
			
			gtk_container_add (GTK_CONTAINER(content), dg)
			gtk_dialog_add_button(GTK_DIALOG(dialog), "OK", GTK_RESPONSE_OK)
			'gtk_dialog_add_button(GTK_DIALOG(dialog), "_Accept", GTK_RESPONSE_ACCEPT)
			
			gtk_widget_grab_focus (dge1)
			'gtk_widget_grab_focus(gtk_dialog_get_widget_for_response(GTK_DIALOG(dialog), GTK_RESPONSE_OK))
			gtk_widget_show_all (dialog)
			response = gtk_dialog_run(GTK_DIALOG(dialog))
			If response=-5 Then
				ibstring=*gtk_entry_get_text(dge1)
			EndIf	
			gtk_widget_destroy(dialog)
			gtk_widget_grab_focus(ebox)
	End Select
	Return ibstring
End Function
Sub calcellipseellipseintersectionold()
	Dim As Double cx1,cy1,cr1,cr2, cx2,cy2,cr3,cr4,erot1,erot2
'	Dim As Double A0,A1,A2,A3,A4,det,fxm1,fxm2,fxm3,fxm4,fym1,fym2,fym3,fym4,erotx,eroty
	Dim As Integer i
'	Dim As Double B1,C1
	Dim As Double c1eplotx1,c1eploty1,c1eplotx2,c1eploty2
	Dim As Double c2eplotx1,c2eploty1,c2eplotx2,c2eploty2
	Dim As Double anglerange

	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	Dim As Double fxmt,fymt,fxmtp,fymtp
	Dim As Double fxmtp1,fymtp1,fxmtp2,fymtp2


	cx1=circles(aintersect,1)'cx1,cy1 are the values of the center of the ellipse
	cy1=circles(aintersect,2)
	cr1=circles(aintersect,4)'cr1,cr2 are the major & minor radius values of the ellipse
	cr2=circles(aintersect,8)
	erot1=circles(aintersect,11)
	
	cx2=circles(bintersect,1)'cx1,cy1 are the values of the center of the ellipse
	cy2=circles(bintersect,2)
	cr3=circles(bintersect,4)'cr1,cr2 are the major & minor radius values of the ellipse
	cr4=circles(bintersect,8)
	erot2=circles(bintersect,11)
	
'	window (wx1,wy1)-(wx2,wy2)
	
	fxmtp=cx1
	fymtp=cy1
	fxmt=mousex
	fymt=mousey
	Dim As Double arx1,ary1,arx2,ary2
	For anglerange=10 To .01 Step -.01

'		arx1=cx1
'		ary1=cy1
		arx2=fxmt
		ary2=fymt
		x1p=arx2-cx1
		y1p=(ary2-cy1)
		arx2=x1p*Cos(-erot1*d2r) - y1p*Sin(-erot1*d2r)+cx1
		ary2=y1p*Cos(-erot1*d2r) + x1p*Sin(-erot1*d2r)+cy1
		ylength=(ary2-cy1)*(cr1/cr2)
		xlength=arx2-cx1
		fixangle
		arcstart=angle -anglerange
		arcend=angle +anglerange
		If arcstart < 0 Then arcstart = 360-arcstart
		If arcend > 360 Then arcend=arcend-360
		If arcstart > arcend Then arcend=arcend + 360
		c1eplotx1=cx1+(Cos(arcstart*d2r)*cr1)
		c1eploty1=cy1+(Sin(arcstart*d2r)*cr2)
		x1p=c1eplotx1-cx1
		y1p=c1eploty1-cy1
		c1eplotx1=x1p*Cos(erot1*d2r) - y1p*Sin(erot1*d2r)+cx1
		c1eploty1=y1p*Cos(erot1*d2r) + x1p*Sin(erot1*d2r)+cy1
		c1eplotx2=cx1+(Cos(arcend*d2r)*cr1)
		c1eploty2=cy1+(Sin(arcend*d2r)*cr2)
		x1p=c1eplotx2-cx1
		y1p=c1eploty2-cy1
		c1eplotx2=x1p*Cos(erot1*d2r) - y1p*Sin(erot1*d2r)+cx1
		c1eploty2=y1p*Cos(erot1*d2r) + x1p*Sin(erot1*d2r)+cy1
'		If anglerange=1 Then
'			Line(c1eplotx1,c1eploty1)-(c1eplotx2,c1eploty2),11
'		Else
'			Line(c1eplotx1,c1eploty1)-(c1eplotx2,c1eploty2),7
'		EndIf
				
'		arx1=cx2
'		ary1=cy2
		arx2=fxmt
		ary2=fymt
		x1p=arx2-cx2
		y1p=(ary2-cy2)
		arx2=x1p*Cos(-erot2*d2r) - y1p*Sin(-erot2*d2r)+cx2
		ary2=y1p*Cos(-erot2*d2r) + x1p*Sin(-erot2*d2r)+cy2
		ylength=(ary2-cy2)*(cr3/cr4)
		xlength=arx2-cx2
		fixangle
		arcstart=angle -anglerange
		arcend=angle +anglerange
		If arcstart < 0 Then arcstart = 360-arcstart
		If arcend > 360 Then arcend=arcend-360
		If arcstart > arcend Then arcend=arcend + 360
		c2eplotx1=cx2+(Cos(arcstart*d2r)*cr3)
		c2eploty1=cy2+(Sin(arcstart*d2r)*cr4)
		x1p=c2eplotx1-cx2
		y1p=c2eploty1-cy2
		c2eplotx1=x1p*Cos(erot2*d2r) - y1p*Sin(erot2*d2r)+cx2
		c2eploty1=y1p*Cos(erot2*d2r) + x1p*Sin(erot2*d2r)+cy2
		c2eplotx2=cx2+(Cos(arcend*d2r)*cr3)
		c2eploty2=cy2+(Sin(arcend*d2r)*cr4)
		x1p=c2eplotx2-cx2
		y1p=c2eploty2-cy2
		c2eplotx2=x1p*Cos(erot2*d2r) - y1p*Sin(erot2*d2r)+cx2
		c2eploty2=y1p*Cos(erot2*d2r) + x1p*Sin(erot2*d2r)+cy2

		ax1=c1eplotx1
		ay1=c1eploty1
		ax2=c1eplotx2
		ay2=c1eploty2
		bx1=c2eplotx1
		by1=c2eploty1
		bx2=c2eplotx2
		by2=c2eploty2
		
		A1 = ay2-ay1
		B1 = ax1-ax2
		C1 = A1*ax1+B1*ay1
		
		A2 = by2-by1
		B2 = bx1-bx2
		C2 = A2*bx1+B2*by1
		
		det = A1*B2 - A2*B1
		fxmt = (B2*C1 - B1*C2)/det
		fymt = (A1*C2 - A2*C1)/det
'		Line (fxmt,fymt)-(fxmtp,fymtp)
		fxmtp=fxmt
		fymtp=fymt
		ipass=0
		Select Case fxmt
			Case c1eplotx1 To c1eplotx2, c1eplotx2 To c1eplotx1
				Select Case fxmt
					Case c2eplotx1 To c2eplotx2, c2eplotx2 To c2eplotx1
						Select Case fymt
							Case c1eploty1 To c1eploty2, c1eploty2 To c1eploty1
								Select Case fymt
									Case c2eploty1 To c2eploty2, c2eploty2 To c2eploty1
										ipass=1
								End Select
						End Select
				End Select
		End Select
		If ipass=0 Then
'			Print "ipass failed"
			Exit For
		EndIf
	
	
	

	Next

	intersection="inside"
	fxm=fxmt
	fym=fymt


End Sub
Sub calccircleellipseintersectionold()
	Dim As Double cx1,cy1,cr1,cr2, cx2,cy2,cr3,cr4,erot1,erot2
'	Dim As Double A0,A1,A2,A3,A4,det,fxm1,fxm2,fxm3,fxm4,fym1,fym2,fym3,fym4,erotx,eroty
	Dim As Integer i
'	Dim As Double B1,C1
	Dim As Double c1eplotx1,c1eploty1,c1eplotx2,c1eploty2
	Dim As Double c2eplotx1,c2eploty1,c2eplotx2,c2eploty2
	Dim As Double anglerange

	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	Dim As Double A1,A2,B1,B2,C1,C2,det
	Dim As Double fxmt,fymt,fxmtp,fymtp
	Dim As Double fxmtp1,fymtp1,fxmtp2,fymtp2

	cx1=circles(aintersect,1)'cx1,cy1 are the values of the center of the ellipse
	cy1=circles(aintersect,2)
	cr1=circles(aintersect,4)'cr1,cr2 are the major & minor radius values of the ellipse
	
	cx2=circles(bintersect,1)'cx1,cy1 are the values of the center of the ellipse
	cy2=circles(bintersect,2)
	cr3=circles(bintersect,4)'cr1,cr2 are the major & minor radius values of the ellipse

	Select Case circles(aintersect,9)
		Case 1,2
			cr2=circles(aintersect,4)
			erot1=0
		Case 3,4
			cr2=circles(aintersect,8)
			erot1=circles(aintersect,11)
	End select
	Select Case circles(bintersect,9)
		Case 1,2
			cr4=circles(bintersect,4)
			erot2=0
		Case 3,4
			cr4=circles(bintersect,8)
			erot2=circles(bintersect,11)
	End Select
	
'	window (wx1,wy1)-(wx2,wy2)
	
	fxmtp=cx1
	fymtp=cy1
	fxmt=mousex
	fymt=mousey
	
	For anglerange=10 To .01 Step -.01

'		x1=cx1
'		y1=cy1
		x2=fxmt
		y2=fymt
		x1p=x2-cx1
		y1p=(y2-cy1)
		x2=x1p*Cos(-erot1*d2r) - y1p*Sin(-erot1*d2r)+cx1
		y2=y1p*Cos(-erot1*d2r) + x1p*Sin(-erot1*d2r)+cy1
		ylength=(y2-cy1)*(cr1/cr2)
		xlength=x2-cx1
		fixangle
		arcstart=angle -anglerange
		arcend=angle +anglerange
		If arcstart < 0 Then arcstart = 360-arcstart
		If arcend > 360 Then arcend=arcend-360
		If arcstart > arcend Then arcend=arcend + 360
		c1eplotx1=cx1+(Cos(arcstart*d2r)*cr1)
		c1eploty1=cy1+(Sin(arcstart*d2r)*cr2)
		x1p=c1eplotx1-cx1
		y1p=c1eploty1-cy1
		c1eplotx1=x1p*Cos(erot1*d2r) - y1p*Sin(erot1*d2r)+cx1
		c1eploty1=y1p*Cos(erot1*d2r) + x1p*Sin(erot1*d2r)+cy1
		c1eplotx2=cx1+(Cos(arcend*d2r)*cr1)
		c1eploty2=cy1+(Sin(arcend*d2r)*cr2)
		x1p=c1eplotx2-cx1
		y1p=c1eploty2-cy1
		c1eplotx2=x1p*Cos(erot1*d2r) - y1p*Sin(erot1*d2r)+cx1
		c1eploty2=y1p*Cos(erot1*d2r) + x1p*Sin(erot1*d2r)+cy1
'		If anglerange=1 Then
'			Line(c1eplotx1,c1eploty1)-(c1eplotx2,c1eploty2),11
'		Else
'			Line(c1eplotx1,c1eploty1)-(c1eplotx2,c1eploty2),7
'		EndIf
				
'		x1=cx2
'		y1=cy2
		x2=fxmt
		y2=fymt
		x1p=x2-cx2
		y1p=(y2-cy2)
		x2=x1p*Cos(-erot2*d2r) - y1p*Sin(-erot2*d2r)+cx2
		y2=y1p*Cos(-erot2*d2r) + x1p*Sin(-erot2*d2r)+cy2
		ylength=(y2-cy2)*(cr3/cr4)
		xlength=x2-cx2
		fixangle
		arcstart=angle -anglerange
		arcend=angle +anglerange
		If arcstart < 0 Then arcstart = 360-arcstart
		If arcend > 360 Then arcend=arcend-360
		If arcstart > arcend Then arcend=arcend + 360
		c2eplotx1=cx2+(Cos(arcstart*d2r)*cr3)
		c2eploty1=cy2+(Sin(arcstart*d2r)*cr4)
		x1p=c2eplotx1-cx2
		y1p=c2eploty1-cy2
		c2eplotx1=x1p*Cos(erot2*d2r) - y1p*Sin(erot2*d2r)+cx2
		c2eploty1=y1p*Cos(erot2*d2r) + x1p*Sin(erot2*d2r)+cy2
		c2eplotx2=cx2+(Cos(arcend*d2r)*cr3)
		c2eploty2=cy2+(Sin(arcend*d2r)*cr4)
		x1p=c2eplotx2-cx2
		y1p=c2eploty2-cy2
		c2eplotx2=x1p*Cos(erot2*d2r) - y1p*Sin(erot2*d2r)+cx2
		c2eploty2=y1p*Cos(erot2*d2r) + x1p*Sin(erot2*d2r)+cy2
'		If anglerange=1 Then
'			Line(c2eplotx1,c2eploty1)-(c2eplotx2,c2eploty2),11
'		Else
'			Line(c2eplotx1,c2eploty1)-(c2eplotx2,c2eploty2),7
'		EndIf

'note: two lines will always intersect unless they are parallel
	'	Dim As Double ax1,ay1,ax2,ay2,bx1,by1,bx2,by2
	'	Dim As Double A1,A2,B1,B2,C1,C2,det
		ax1=c1eplotx1
		ay1=c1eploty1
		ax2=c1eplotx2
		ay2=c1eploty2
		bx1=c2eplotx1
		by1=c2eploty1
		bx2=c2eplotx2
		by2=c2eploty2
		
		A1 = ay2-ay1
		B1 = ax1-ax2
		C1 = A1*ax1+B1*ay1
		
		A2 = by2-by1
		B2 = bx1-bx2
		C2 = A2*bx1+B2*by1
		
		det = A1*B2 - A2*B1
		fxmt = (B2*C1 - B1*C2)/det
		fymt = (A1*C2 - A2*C1)/det
'		Line (fxmt,fymt)-(fxmtp,fymtp)
		fxmtp=fxmt
		fymtp=fymt
	
		ipass=0
		Select Case fxmt
			Case c1eplotx1 To c1eplotx2, c1eplotx2 To c1eplotx1
				Select Case fxmt
					Case c2eplotx1 To c2eplotx2, c2eplotx2 To c2eplotx1
						Select Case fymt
							Case c1eploty1 To c1eploty2, c1eploty2 To c1eploty1
								Select Case fymt
									Case c2eploty1 To c2eploty2, c2eploty2 To c2eploty1
										ipass=1
								End Select
						End Select
				End Select
		End Select
		If ipass=0 Then
'			Locate 50,50
'			Print "ipass failed"
			Exit For
		EndIf
	
	
	'	If anglerange=19 Then
	'		fxmtp1=fxmt
	'		fymtp1=fymt	
	'	EndIf
	'	If anglerange=1 Then
	'		fxmtp2=fxmt

	'		fymtp2=fymt	
	'	EndIf

	Next
'	Line (fxmtp1,fymtp1)-(fxmtp2,fymtp2),13

	intersection="inside"
	fxm=fxmt
	fym=fymt
End Sub
Sub fixangle()
	'R = D * PI / 180
	'D = R * 180 / PI
	angle=atan2(ylength,xlength)*r2d
	If angle<0 Then angle+=360
End Sub
Sub fixperpangle()
	perpangle=atan2(ylength,xlength)*r2d
	If perpangle<0 Then perpangle+=360
End Sub
Sub fixflangle()
	flangle=atan2(ylength,xlength)*r2d
	If flangle<0 Then flangle+=360
End Sub
Sub fixslangle()
	slangle=atan2(ylength,xlength)*r2d
	If slangle<0 Then slangle+=360
End Sub
Sub fixselangle()
	selangle=atan2(ylength,xlength)*r2d
	If selangle<0 Then selangle+=360
End Sub


'button data - limit total number of character between the short and long descriptions to 184
'data number, display text, initial state, short description, long description
data 1,"L","on","draw lines","clicking the mouse once starts the line. move the mouse and continue left clicking to draw more lines. press esc to stop drawing lines."
data 2,"C","off","draw circles","description - write me"
data 3,"A","off","draw arcs","description - write me"
data 4,"E","off","draw ellipses","description - write me"
data 5,"e","off","draw elliptical arcs","description - write me"
data 6,"S","off","draw splines","clicking the mouse once starts the spline. move the mouse and continue left clicking to draw more lines. press esc to stop drawing lines."
Data 11,"S","on","snap to end point","with snap on, you can connect lines to the end points of other lines. turn snap off in order to avoid connecting to end points of other lines."
data 12,"M","off","snap to mid point","turn mid point on to detect and snap to the mid point of a line."
data 13,"P","off","snap to perpendicular","turn perpendicular on to detect and snap to points that are perpedicular to other lines."
data 14,"T","off","snap to tangent","while drawing a line, you can snap to the tangent of a circle, arc, ellipse or elliptical arc"
data 16,"C","off","snap to center","snaps to the center of arcs, ellipses, and circles"
data 18,"I","off","Snap to Intersection","Snap to the intersecton of lines"
Data 19,"N","off","Snap to Nearest Point","Snap to the nearest point of another object"
Data 20,"A","off","Snap to arcs intercept","Snaps to the calculated intersection of an arc and other objects"
data 21,"O","off","ortho","turn ortho mode on or off"
data 22,"A","off","draw at an angle","either select the angle of another line or enter the angle using the keyboard."
data 23,"s","off","Set user angles","Seven user angles are available to set."
Data 24,"1","off","User set angle1=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
Data 25,"2","off","User set angle2=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
Data 26,"3","off","User set angle3=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
Data 27,"4","off","User set angle4=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
Data 28,"5","off","User set angle5=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
Data 29,"6","off","User set angle6=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
Data 30,"7","off","User set angle7=","With Angle enabled AND 1 of the 7 user angles enabled, lines are drawn or objects are moved at specified angle."
data 31,"M","off","move","move selected group from point to point"
data 32,"C","off","copy","coppies and moves selected group from point to point"
data 33,"R","off","rotate","rotates selected group at pivot point to angle"
data 34,"r","off","copy-rotate","coppies and rotates selected group from pivot point to angle"
data 35,"V","off","flip vertical","flips selected group vertically at point"
data 36,"H","off","flip horizontal","flips selected group horizontally at point"
data 37,"v","off","copy flip vertical","coppies and flips selected group vertically from point to point"
data 38,"h","off","copy flip horizontal","coppies and flips selected group horizontally from point to point"
data 39,"B","off","flip v & h","flips selected group vertically & horizontally at point"
data 40,"b","off","copy flip v & h","coppies and flips selected group vertically & horizontally from point to point"
Data 41,"Z","off","zoom in","zoom in on the drawing - rotating the mouse wheel in the drawing area does the same"
Data 42,"z","off","zoom out","zoom out on the drawing - rotating the mouse wheel in the drawing area does the same"
Data 43,"P","off","pan","pan the drawing left,right,up,down,diagonally by holding the left mouse buttun down and dragging"
Data 44,"E","off","view extents","zoom out to the extents of the drawing"
Data 45,"S","off","Save view","Saves current view"
Data 46,"<","off","Previous view","Changes veiw to previous saved view"
Data 47,">","off","Next view","Changes veiw to next saved veiw"
Data 48,"F","off","First view","Changes veiw to first saved veiw"
Data 49,"L","off","Last view","Changes veiw to last saved veiw"
Data 50,"d","off","Delete view","Deletes current saved view if viewing a saved view"
Data 51,"M","off","Move Point","Moves the end point of a line"
Data 52,"J","off","Join Points not working yet","Joins or sets the endpoints of two lines to the same point"
Data 53,"P","off","Perpendicular From","Temporarily sets Angle perpendicular to the selected line or circle"
Data 54,"T","off","Tangent From","Temporarily sets Angle tangent to the selected circle"
Data 55,"P","off","Parallel single","Creates lines and arcs parallel to objects - which side the paralles are create on is determined by positioning the mouse relative to the initial point of selection"
Data 56,"B","off","Parallel dual","Creates lines and arcs parallel to objects on both sides at the same time."
Data 57,"s","off","Set user offsets","Sets the offset values for the 3 buttons to the right"
Data 58,"1","off","User set offset1=","With this button enabled, parallels are created using it's offset value"
Data 59,"2","off","User set offset2=","With this button enabled, parallels are created using it's offset value"
Data 60,"3","off","User set offset3=","With this button enabled, parallels are created using it's offset value"
Data 61,"T","off","Trim","Trims objects - Select objects first, turn trim on and then click object(s) to trim"
Data 62,"E","off","Extend - not working yet","Extends objects - Select objects first, turn extend on  and then click object(s) to extend"
Data 65,"E","off","Extend exterior parallel corners","Lines are extended in exterior corners when building parallel paths of a group"
Data 66,"F","off","Fillet exterior parallel corners","Arcs are created in exterior corners when building parallel paths of a group"
Data 71,"D","off","Dimension","Dimension of distance between two points"
Data 72,"X","off","X Dimension","Dimension of distance on the X axis"
Data 73,"Y","off","Y Dimension","Dimension of distance on the Y axis"
Data 74,"R","off","Radius Dimension","Dimension of radius for circles and arcs."
Data 75,"D","off","Diameter Dimension","Dimension of diameter for circles."
Data 76,"A","off","Angle Dimension","Dimension of an angle using the inside of the angle to show the amount of degrees of arc."
'Data 77,"a","off","Angle Dimension outside","Dimension of an angle using the outside of the angle to show the amount of degrees of arc."
Data 78,"P","off","Dimension Precision=","Enter the decimal precision for the dimensions value. For example entering 2 will force 1.234 to 1.23"
Data 79,"A","off","Dimension Arrow Size=","Set the size of the dimension arrows."
Data 80,"L","off","Dimension Leader Offset=","Set the leader offset."
Data 81,"C","off","Chamfer","Chamfer or bevel. Select two lines first then chamfer."
Data 82,"F","off","Fillet","Fillet or round off. Select two lines first then fillet."
Data 83,"B","off","Best curve fit","Creates a curve best fit. Select two lines first then fillet."
Data 84,"f","off","Free hand: chamfer/fillet","With Chamfer or Fillet enabled the chamfer/fillet enabled, two lines are chamfered / fillet at a length determined by tracking the mouse cursor relative to the two line's common point."
Data 85,"s","off","Set user chamfer/fillet lengths","Five user chamfer/fillet lengths are available to set."
Data 86,"1","off","User set chamfer/fillet length1=","With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
Data 87,"2","off","User set chamfer/fillet length2=","With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
Data 88,"3","off","User set chamfer/fillet length3=","With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
Data 89,"4","off","User set chamfer/fillet length4=","With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
Data 90,"5","off","User set chamfer/fillet length5=","With Chamfer or Fillet enabled AND 1 of the 5 user chamfer/fillet lengths enabled, two lines are chamfered / fillet at specified length."
Data 91,"T","off","TexT","Insert text. Enter text string"
Data 92,"S","off","Text Size=","Enter the size for the text starting at 1 to 100 or more." 
Data 101,"S","off","Scale","Enter the amout to scale selected objects. To scale down enter a value less then 1 such as .5 or .25 or less. To scale up enter a value larger then 1 such as 1.5 or 2 or more."
Data 102,"F","off","Scale Factor=","Enter the scale factor used when scaling up or down via the scale up or down buttons or keyboard shortcuts."
Data 103,"U","off","Scale UP","Sellected objects increase in scale according to the Scale Increment."
Data 104,"D","off","Scale DOWN","Sellected objects decrease in scale according to the Scale Increment."
Data 105,"I","off","Individual Axis Scaling","Scale Individual axis overrides Scale Factor. While enabled - scaling up or down will scale xyz axis according to indivual axis settings."
Data 106,"x","off","Scale X Factor=","Enter the X scale factor to be used when scaling up or down. Default is 1."
Data 107,"y","off","Scale Y Factor=","Enter the Y scale factor to be used when scaling up or down. Default is 1."
Data 108,"z","off","Scale Z Factor=","Enter the Z scale factor to be used when scaling up or down. Default is 1."
Data 109,"s","off","Scale with grab handles","Scale by moving 1 of the 8 grab handles encompasing the selected objects" 
Data 111,"B","off","Block","Enter a name for the new block. Selected entities are saved as a BLOCK"
Data 112,"I","off","Block Insert","Enter the name of a block to insert"
Data 113, "b","off","Block Base Point Enabled","With this enabled (on) the base point is set to insertionxy (last left mouse click in drawing area)"
Data 114, "X","off","explode Block","Selected block(s) are changed from a block (or group) to individual entities (or objects)."
Data 115,"W","off","Wblock","Write block creates a block and writes it to file at the same time"
Data 116, "E","off","Edit block","Edit the selected block"
Data 121,"G","off","Grid","Truns the grid visibly on or off"
Data 122,"S","off","Snap to Grid","Snap to grid even if it is visibly off"
Data 123,"X","off","Grid X spacing=","Set the grid's X spacing - default is 10 units"
Data 124,"Y","off","Grid Y spacing=","Set the grid's Y spacing - default is 10 units"
Data 125,"x","off","Grid x offset=","Set the grid's x offset - default is 0"
Data 126,"y","off","Grid y offset=","Set the grid's y offset - default is 0"
Data 131,"T","off","Trajectory Plot","Based on trajectory settings - Plots trajectory from point of first mouse click to virtical distance from that point"
Data 132,"U","off","Units per meter=","Set the number of fbcad units that represent one meter"
Data 133,"G","off","Gravity=","Set Gravity default is 9.81"
Data 134,"V","off","Velocity=","Set initial velocity"
Data 135,"t","off","Theta","Set theta"
'Data 136,"I","off","Initial point","Select Initial starting point of tracjectory"
'Data 137,"v","off","Vertical Distance","Enter the vertical distance from Yo to Yf"
'
'so far this gcode is for very basic cut on flat surface
Data 141,"G","off","G-code","Generates g-code for a path defined by selecting end points of objects"
Data 142,"M","off","G-code properties",""
Data 143,"U","off","Not used yet","Reserved for future gcode functions"
Data 144,"D","off","Not used yet","Reserved for future gcode functions"

Data 0
'menu
Data "File"
Data "-New"
Data "-Open"
Data "-Save"
Data "-Save As"
Data "-Save & EXIT"
Data "-Exit"
Data "Settings"
Data "-User Preferences"
Data "Help"
Data "-FB Cad Help"
Data "-About"
Data "end menu"
