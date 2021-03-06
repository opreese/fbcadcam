Declare Function mouse_button_press_events(ByVal widget as GtkWidget ptr, byval event as GdkEventButton ptr) as gboolean
Declare Function mouse_button_release_events(ByVal widget as GtkWidget ptr, byval event as GdkEventButton ptr) as gboolean
Declare Function mouse_scroll_event(byval widget as GtkWidget ptr, byval event as GdkEventScroll ptr) as gboolean
Declare Function mouse_motion_notify_event Cdecl ( Byval widget As GtkWidget Ptr, Byval event As GdkEventMotion Ptr ) As Integer
Declare Function mouse_enter_event Cdecl ( Byval widget As GtkWidget Ptr, Byval event As GdkEventMotion Ptr ) As Integer

Declare Function shortcut_keys Cdecl(byval widget as GtkWidget ptr, byval event as GdkEventKey ptr) as gboolean
Declare Function disable_shortcut_keys Cdecl(byval widget as GtkWidget ptr, byval event as GdkEventKey ptr) as gboolean

Declare Sub toolbar_button_new_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_open_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_save_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_save_as_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_cut_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_copy_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_paste_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_delete_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_undo_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_redo_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_zoom_in_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_zoom_out_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_zoom_fit_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_zoom_100_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_macro_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
Declare Sub toolbar_button_help_op cdecl(byval widget as GtkWidget ptr, byval item as gpointer)
'/button area on left hand side of screen - sub routine declarations*****
Declare Sub drawing_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub snap_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub ortho_angle_from_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub mcrf_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub zpv_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub dim_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub cham_fil_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub scale_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub block_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub grid_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub gcode_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub misc_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub parallel_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub text_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
'add code for button area - declare new sub routine
'/*****

Declare Sub button_enter Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae2gsb_enter Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae2gsb_leave Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae2gsbadj_value_changed Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae2gsbadj_config(value As Integer,upper As Integer)
Declare Sub pae2gb_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Function pae2ge_click Cdecl(Byval widget As GtkWidget Ptr, byval event as GdkEventButton ptr, Byval dat As gpointer) As gboolean

Declare Sub pae3gsb_enter Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae3gsb_leave Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae3gsbadj_value_changed Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub pae3gsbadj_config(value As Integer,upper As Integer)
Declare Sub pae3gb_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Function pae3ge_click Cdecl(Byval widget As GtkWidget Ptr, byval event as GdkEventButton ptr, Byval dat As gpointer) As gboolean

Declare SUB filtertype_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)






Declare Sub load_button_desc()
Declare SUB layer_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare SUB layer_state_button_clicked Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)
Declare Sub baeg_swap Cdecl(Byval widget As GtkWidget Ptr, Byval dat As gpointer)


Declare Sub myfbgfxtogtkimg()
Declare function mymain() As gboolean
Declare Sub mygfxcolors()
Declare Sub myrgbcol(pc As UByte)
Declare Sub drawlines()
Declare Sub drawcircles()


Declare Sub buttonmanager()
Declare Sub turn_gtk_button_off()
Declare Sub turn_gtk_button_on()






declare Sub init_fbcadcam()
declare Sub main()
declare sub initlinedraw()
declare sub savedxf(saveasfilename As String)
declare sub savedxfold(saveasfilename As String)
Declare sub determinedrawmode2()
declare sub determinedrawmode1()
declare sub createline()
declare sub createnewmodlines()
declare sub memmanageline()
declare sub beginnewthing()
declare sub detectpoints()
declare sub hilisnap()
declare sub group()
declare sub showgroups()
declare sub trackangle()
declare sub tracklength()
declare sub calcselangle()
declare sub calcsellength()
declare sub calcperpendicular()
declare sub altosellength()
declare sub altox2y2()
declare sub altofxm()
declare sub altofym()
Declare sub aftom()
declare sub atolength()
declare sub atolength2()
declare sub redraw()
declare sub getkeybd()
declare sub importdxf()
declare sub forcexyz()
Declare sub calcmidpoint()
Declare sub turnbuttonoff()
declare sub turnbuttonoff2()
Declare sub turnbuttonon()
declare sub orthomode()
declare sub escapeme()
declare Sub escapeall()
declare sub drawlineatangle1()
declare sub drawlineatangle2()
declare sub movegroup()
declare sub setmovedown()
declare sub setcopydown()
declare sub degroup()
declare sub rotategroup()
declare sub setrotatedown()
declare sub setrotatecopydown()
Declare Sub rotateblock(bntr As Integer)
declare sub flipvertical()
declare sub fliphorizontal()
declare sub flipverticalcopy()
declare sub fliphorizontalcopy()
declare sub flipvh()
declare sub flipvhcopy()
Declare Sub zoomin()
Declare Sub zoomout()
Declare Sub pan()
Declare Sub zoominpan()
Declare Sub zoomoutpan()
Declare Sub panpointtomouse(x As integer,y As Integer)
Declare Sub zoomextents()
Declare Sub groupbox(g As Integer,s As Integer)
Declare Sub adjustextents()
Declare Sub adjustgextents()
Declare Sub createcircle()
Declare Sub memmanagecircle()
Declare Sub createarc()
Declare Sub calcarcendpoints()
Declare Sub calcarcendpoints2(i As Integer)
Declare Sub calcarcmidpoint()
Declare Sub calcarcmidpoint2(i As Integer)
Declare Sub createnewmodcircles(j As Integer)
Declare Sub selarcangle()
Declare Sub plotellipse(ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double)
Declare Sub plotellipse_gcode(ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double)
Declare Function calcellipsenorm(ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double) As Double
Declare Function calcellipsenorm2(poex As Double,poey As Double,foci1x As Double,foci1y As Double,foci2x As Double,foci2y As Double) As Double
Declare Sub createellipse()
Declare Sub calcellipsarcendpoints(i As Integer)
Declare Sub boxarc(j As Integer,ex1 As Double,ey1 As Double,ez1 As Double,er1 As Double,ecolor As Integer,estart As Double,eend As Double,er2 As Double,eangle As Double)
Declare Sub calcellipsarcmidpoint(i As Integer)
Declare Sub enablebutton()
Declare Sub calcnearestpoint()
Declare Sub setdimensiondown()
Declare sub createanchar(anchar As Integer, ancharx As Double, anchary As Double)
Declare function calcd(cd1 as Double,cd2 as Double,cd3 as Double,cd4 as Double,cd5 as Double,cd6 as Double) as double
declare function getbuttonid(x as Integer,y as Integer) As Integer
Declare Sub csntodec()
Declare Sub movepoints()
Declare Sub movepointstomouse()
Declare Sub setmovepointsdown()
Declare Sub calclinelineintersection()
Declare Sub calclinelineintersection2(ax1 As Double,ay1 As Double,ax2 As Double,ay2 As Double,bx1 As Double,by1 As Double,bx2 As Double,by2 As Double)
Declare Sub calclli(ax1 As Double,ay1 As Double,ax2 As Double, ay2 As Double,bx1 As Double,by1 As Double,bx2 As Double, by2 As Double)
Declare Sub scanpoints()
Declare Sub calclinecircleintersection()
Declare Sub calclineellipseintersection()
Declare Sub calccirclecircleintersection()
Declare Sub calcellipseellipseintersectionold()
Declare Sub calccircleellipseintersectionold()
Declare Sub calcellipseellipseintersection()
Declare Sub calccircleellipseintersection()
Declare Sub trimobjects()
Declare Sub extendobjects()
Declare Sub calcperpintersects(px1 As Double,py1 As Double,pa1 As Double,px2 As Double,py2 As Double,pa2 As Double)
Declare Sub calcellipseintersects(px1 As Double,py1 As Double,pa1 As Double,px2 As Double,py2 As Double,pa2 As Double)
Declare Function abtp(x1 as Double,y1 as Double,z1 as Double,x2 as Double,y2 as Double,z2 as Double) as double
Declare Sub importblockdxf(bfn As String)
Declare Sub plottextstring(ptstring As String, xoffset As Double, yoffset As Double, fontsize as Integer)
Declare Sub scaleit(objecttype As String,objectarrayvalue As Integer, scalexamount As Double, scaleyamount As Double, scalezamount As Double, scalefromx As Double, scalefromy As Double)
Declare Sub inview()
Declare Sub inviewfilter()
Declare Sub calcellipsepoint(cepi As Integer)
Declare sub saveblockdxf()
Declare Sub hiliblock(blocknumber As Integer)
Declare sub importblock()
Declare Sub adjustprecision()
Declare Sub calctangentpoint()
Declare Sub calcvectorangle(vx1 As Double, vy1 As Double, vx2 As Double, vy2 As double)
Declare Sub calclei(ax1 As Double,ay1 As Double,ax2 As Double,ay2 As Double,cx1 As Double,cy1 As Double,cr1 As Double,cr2 As Double,erot As double)
Declare Sub snaptogrid()
Declare Sub initoptionboxeswindow()
Declare Sub movesby()
Declare Sub movesbyi(msbyi As Integer)
Declare Sub initoptionboxes()
Declare Sub addopptionbox(param1 As Integer,param2 As Integer,param3 As Integer,param4 As String)
Declare Sub scrolloptionboxes()
Declare Sub drawoptionboxes()
Declare Sub datagrid(dgtop As Integer,dgleft As Integer,dghight As Integer,dgwidth As Integer,dgcolumbsc As Integer,dgrowsc As Integer)
Declare Sub detectoptionboxes()
Declare Sub dxfheader1()
Declare Sub dxfheader2()
Declare Sub dxfheader3()
Declare Sub dxfclasses()
Declare Sub dxftablevport()
Declare Sub dxftablevportold()
Declare Sub dxftableltype()
Declare Sub dxftablelayers()
Declare Sub dxftablelayersold()
Declare Sub dxftableviewucsappiddimstyle()
Declare Sub dxftableblocks()
Declare Sub dxfblockpreamble()
Declare Sub dxfbuildblocks(dxfblocki As Integer)
Declare Sub dxfblockentities(dxfblocknumber As Integer)
Declare Sub dxfentities()
Declare Sub dxfdictionarys()
Declare Sub movedxfblock(mdbi As Integer)
Declare Sub flipellipseaxis(flipfilename As String)
Declare Sub theboxbelow(txt As String)
Declare Sub infobox(txt As String)
Declare Sub copybaseblock(cbbi As Integer)
Declare Sub monkeysmatter(mmrefbi As Integer, mmbi As Integer)
Declare Sub monkeysmatter2(mmbi1 As Double,mmbi2 As Double,mmbi4 As Double,mmbi5 As Double)
Declare Sub scalegroup()
Declare Sub setscaledgroupdown(tempgscalexfactor As Double, tempgscaleyfactor As Double, tempgscalezfactor As Double)
Declare Sub escapegroupscaling()
Declare Sub calctrajectory()
Declare Sub plottrajectory()
Declare Function calccmr(cmrt As Double, cmr1 As Double,cmr2 As Double,cmr3 As Double,cmr4 As Double) as Double
Declare Sub showcurve()
Declare Sub curvesetup()
Declare Sub duplicate(duplicatemethod As string)
Declare Function mymod(n As Double,m As Integer) As Double
Declare Sub countactiveobjects()
Declare Sub textviewer(texttoview As String)
Declare Sub newdrawing()
Declare Sub cpeforlines(line1 As Integer,line2 As Integer)
Declare Sub multichamferfillet(multicf As String)
Declare Sub multifillet()
Declare Sub trimlinesincf(line1 As Integer,line2 As Integer)
Declare Sub setchamferdown(line1 As Integer,line2 As Integer)
Declare Sub setfilletdown(line1 As Integer,line2 As Integer)
Declare Sub buildchamfer()
Declare Sub buildfillet()
Declare Sub findallselectedentities()
Declare Sub buildselectedentityarrays()
Declare Sub parallelcontour()
Declare Function inputbox(datatype As String,prompt As String,initvalue As String) As String
Declare Sub loadpic
Declare Sub buttonstatus()
Declare sub setxyz()
Declare Sub setx1()
Declare Sub sety1()
Declare Sub setx2()
Declare Sub sety2()
Declare Sub showview()
declare sub memmanageviews()
declare Sub rotatepoint(rptx As Double,rpty As Double,rpivotx As Double,rpivoty As Double,rangle As Double)
Declare Sub show_polygon(pnos As Integer)
Declare Sub set_polygon_down(pnos As Integer)
Declare Sub draw_right_triangle()
Declare Sub draw_rectangle()
Declare Sub draw_parallelogram()
Declare Sub draw_rhombus()
Declare Sub fixangle()
Declare Sub fixperpangle()
Declare Sub fixflangle()
Declare Sub fixslangle()
Declare Sub fixselangle()
Declare Sub delete_group()
Declare Sub explode_block()


Declare Sub gcode_plotellipse(ex1 As Double,ey1 As Double,er1 As Double,estart As Double,eend As Double,er2 As Double,eangle As Double)
Declare Sub gcode_showpath()
Declare Sub gcode_generate_g_code()
Declare Sub gcode_sort_the_glines()
Declare Sub gcode_remove_duplicates()
Declare Sub gcode_create_fbcadcam_lines()