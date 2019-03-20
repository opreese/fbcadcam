Const pi As double = 4 * Atn(1)
Const d2r As Double = pi/180
Const r2d As Double = 180/pi
Dim Shared As Integer tempintarray()
Dim Shared As Double tempdblarray()
Dim Shared As String tempstrarray()
Dim shared As String crlf
Dim shared As Double fx,fxm,fxd,fy,fym,fyd,fz,fzm,fzd
Dim Shared As Double x1p,x2p,y1p,y2p
Dim Shared As Double x1,y1,z1,x2,y2,z2
Dim shared as double angle,length,xlength,ylength,selangle,sellength,angle2
dim shared as double perpangle,perpangle1,lastangle,newlength,pfxm,pfym
Dim Shared As Double paraselline,paraselcircle,para_perp_angle,para_radius,para_offset,para_direction
Dim shared as integer mousex, mousey, tempmousex, tempmousey, mousez, mousew, mouseb
Dim Shared As Integer damx,damy
Dim Shared As BOOLEAN mousepressed, mousereleased 
dim shared as Double lines()'1=x1 2=y1 3=z1 4=x1 5=y2 6=z2 7=color/layer 8=group 9=block
Dim Shared As Integer linec,linememstep
dim shared as integer mousexp,mouseyp,mousezp,mouseyz,mousewp,mousextemp,mouseytemp
Dim shared as BOOLEAN drawatangle,up, down, drawmode, drawing, ortho, forcex, forcey, forcez, forcedx, forcedy, forcedz, snap
dim shared as integer lc,layerstate(255),layer_active
Dim Shared As Double  detect,detectsize
dim shared as integer autocadcolor
dim shared as BOOLEAN selentity
dim shared as integer dpmx, dpmy, pp
dim shared as integer selline
dim shared as string newanglestring, tempstring
dim shared as integer selbutton',selbuttonx,selbuttony,selbuttonp
dim shared as BOOLEAN buttonson(1230)
'dim Shared as BOOLEAN buttonclick
dim Shared as integer tempint,tempintc
dim shared as double rotatex,rotatey,rotatez
dim shared as integer modify
dim shared as double modifyx1,modifyy1,modifyz1,modifyx2,modifyy2,modifyz2
dim shared as BOOLEAN modifying,groupexists
dim shared as BOOLEAN snapenable
dim shared as integer sellinep,selcirclep
Dim Shared As integer wx1,wy1,wx2,wy2,wzoom,wx1p,wy1p,wx2p,wy2p,wzoomp,wzoomc,wzoomt,maxzoomin,maxzoomout
Dim shared As Integer panx,pany,panxd,panyd
Dim shared As BOOLEAN panning
Dim Shared As Double panxf,panyf
Dim Shared As Double extentsx1,extentsx2,extentsy1,extentsy2
Dim Shared As Double gextentsx1,gextentsx2,gextentsy1,gextentsy2
Dim Shared As BOOLEAN dp
Dim shared As Integer scrnmx,scrnmy
Dim Shared As BOOLEAN boxselect,boxing
Dim Shared As Integer boxselectx1,boxselecty1,boxselectx2,boxselecty2
Dim Shared As Double drawareax1,drawareay1,drawareax2,drawareay2
Dim Shared As Double circles() '1x,2y,3z,4r,5color,6start,7end,8aspect,9=1(circle)2(arc)3(ellips)4(ellipticalarc),10group,11 is ellipse rotation , 12 block
Dim Shared As Integer circlec,circlememstep
Dim Shared As Double arcangle,aspect,arcstart,arcstarttemp,arcend,radius,arcendpoint1x,arcendpoint1y,arcendpoint2x,arcendpoint2y,arcmidpointx,arcmidpointy
Dim Shared As BOOLEAN arcstarted,arcing,arcsetradius
Dim Shared As Double tempdouble
Dim Shared As Integer selcircle
Dim Shared As Double erotation,eradius
Dim Shared As Integer shift_key,alt_key,ctrl_key
Dim Shared As Double circlex,circley
Dim Shared As BOOLEAN ellipsstarted,ellipsing,ellipsarcing
Dim Shared As Double boxarcx1,boxarcx2,boxarcx3,boxarcx4,boxarcy1,boxarcy2,boxarcy3,boxarcy4
Dim Shared As BOOLEAN movingpoints, movingpts
Dim Shared As Double mpfxm,mpfym
Dim Shared As String intersection
Dim Shared As Integer aintersect,bintersect
Dim Shared As Integer tesp
Dim Shared As String tesptype
Dim Shared As Integer multic
Dim Shared As Double multifxm(),multifym()
Dim Shared As Double clifxm1,clifxm2,clifym1,clifym2
Dim Shared As Integer clic,trimlinei
Dim Shared As Double ipointsx(4),ipointsy(4)
Dim Shared As Double ipointsa(4)
Dim Shared As Integer tci'trim circle i
Dim Shared As Integer ccic,ipass
Dim Shared As String otd,fromotd
Dim Shared As Integer dimensioningclicks,ca
Dim Shared As BOOLEAN dimensioning
Dim Shared As Double dimensionxy(6,3)
Dim Shared As String dimvaluestring
Dim Shared As Integer mouse_clicks
Dim Shared As Double curvexy(3,3)
Dim Shared As Double foci
Dim Shared As Integer blockc
Dim Shared As String blockname
Dim Shared As Double blockoffsets()'(blockc,7) 1=insertionx,2=insertiony,3=insertionz,4=scalex,5=scaley,6=scalez,7=rotation
Dim Shared As Byte blockstatus()
Dim Shared As Integer blocklevelc,blocklevel()
Dim Shared As Double ancharwidth
Dim Shared As BOOLEAN loadchars,loadbasicblocks
Dim Shared As Double scalefactor,scalexfactor,scaleyfactor,scalezfactor
Dim Shared As Integer textsize
Dim Shared As Double insertionx,insertiony,insertionz
Dim Shared As Integer inviewlinesc,inviewlines(), inviewcirclesc,inviewcircles()
Dim Shared As Integer inviewfilterlinesc,inviewfilterlines(), inviewfiltercirclesc,inviewfiltercircles()
Dim Shared As Integer detectingpoints
Dim shared As Double epastart,epaend
Dim Shared As BOOLEAN skipecheck
Dim Shared As Double dimleaderspacing,dimleadertextoffsetspacing,dimarrowsize,dimvalue
Dim Shared As Integer tempselcircle
Dim Shared As Integer tempselline
Dim Shared As Double temparcstart,tempx2,tempy2
Dim Shared As Integer dimprecision
Dim Shared As BOOLEAN showellipsefoci,plotellipseoffset
Dim Shared As BOOLEAN createdxfblockdata,savedxfblockonly
Dim Shared As Integer dxfblockc,savedxfblockonlyi
Dim Shared As Integer dxfblockn()
Dim Shared As Integer dxfhandlec,dxfhandlet
Dim Shared As Double gridxspacing,gridyspacing,gridxoffset,gridyoffset
Dim Shared As Double gfxm,gfym
Dim Shared As Integer sby,msbyoffset
Dim Shared As BOOLEAN sbmda,redrawoptionboxes
Dim Shared As Integer optionboxc,obht,optionboxdetected,optionboxdetectedp
Dim Shared As Integer optionbox()'(optionboxnumber,boxheight and minimized=0 or maximized=1)
Dim Shared As Integer activeoptionboxx1,activeoptionboxy1,activeoptionboxx2,activeoptionboxy2,activeoptionboxminmaxx1,activeoptionboxminmaxy1,activeoptionboxminmaxx2,activeoptionboxminmaxy2
Dim Shared As Integer activeoptionboxx1p,activeoptionboxy1p,activeoptionboxx2p,activeoptionboxy2p
Dim Shared As String optionboxtitle()
Dim Shared As Integer grouplinec, groupcirclec, tempgrouplinec, tempgroupcirclec
Dim Shared As Integer grouplines(), groupcircles()
Dim Shared As BOOLEAN groupgrab, groupscaling, settinggroupscaledown
Dim Shared As String groupgrabhandle
Dim Shared As Double groupgrabhandlexy(9,2)'1 thru 8 are the handles and 9 is the calculated center point of the group
Dim Shared As Integer asdfc
Dim Shared As Double gravity,fbcadupm,trajiv,trajtheta,trajix,trajiy,trajiz
Dim Shared As BOOLEAN splinestarted,splining,spliningmovingpoints,setsplinedown
Dim Shared As Integer splinec,splinepoint(100)
Dim Shared As BOOLEAN reversed,reversed2
Dim Shared As Double splinemovingpointsx1,splinemovingpointsy1
Dim Shared As Double fourpoints(4,3)'x,y,z
Dim Shared As BOOLEAN cmrxcross
Dim Shared As Double cmrmaxx,cmrmaxy,cmrxcrossx,cmrxcrossy
Dim Shared As Integer tempcounter
Dim Shared As Double usersetangles(7),usersetoffsets(3)
Dim Shared As Integer linesingroupc,circlesingroupc,singlelinen,singlecirclen,firstlineingroup,secondlineingroup,firstcircleingroup,secondcircleingroup
Dim Shared As BOOLEAN optionboxesredrawn,optionboxhasdata
Dim Shared As Integer mouseinlayer,mouseinlayerp
Dim Shared As Integer tbblcx,tbblcy'the box below last cursor x,y
Dim Shared As Integer ff,gff'freefile gff is gcode free file
Dim Shared As String fbcadver,openeddrawingname
Dim Shared As Integer activelinesc,activecirclesc,activeblocksc
Dim Shared As String fontpath
Dim Shared As BOOLEAN createcircleset
Dim Shared As Integer drawingcirclesmethod
Dim Shared As Integer fontlinec,fontcirclec,fontblockc
Dim Shared As Double cpx,cpy,flangle,slangle,flfx,flfy,slfx,slfy,flx1,fly1,flx2,fly2,slx1,sly1,slx2,sly2
Dim Shared As BOOLEAN cpe'common point exists
Dim Shared As Integer cpfl,cpsl
Dim Shared As Double cflength,useusersetcflengths(5)
Dim Shared As BOOLEAN useusersetcflength
Dim Shared As Integer preloadedlinec,preloadedcirclec
Dim Shared As BOOLEAN proceed
Dim Shared As Double gplotx(360),gploty(360),gplotz(360)
Dim Shared As Integer gplotc
Dim Shared As BOOLEAN gplotoffset'cutting bit follows parallel curve plot
Dim Shared As Double gplotoffsetr'r=radius of bit
Dim Shared As String gplotoffsetd'd=direction
Dim Shared As Double initradius,initlc,initarcstart,initarcend,initeradius,initerotation
Dim Shared As Double eliptolerance,jointolerance
Dim Shared As BOOLEAN showentitytype
Dim Shared As Integer selectedentitiestotal,selectedentitieslinecount,selectedentitiescirclecount=0
Dim Shared As BOOLEAN selectedentitiesexists
Dim Shared As Integer selectedentitylines(),selectedentitycircles()
Dim Shared As Integer joinedentityorder()
Dim Shared As BOOLEAN paraside,paralinesideset
Dim Shared As String entitycircuit
Dim shared As BOOLEAN intersectioninsidebothlines,intersectioninsidefirstline,intersectioninsidesecondline
Dim Shared As BOOLEAN instructionaid
Dim Shared As BOOLEAN drawingareaupdate
Dim Shared As Integer actmx,actmy
Dim Shared As String filtertype()
'Dim Shared As Integer redrawentityc
Dim Shared As BOOLEAN usemyguicalc
Dim Shared As BOOLEAN absrel 'true=absolute false=relative
Dim Shared As BOOLEAN forcelength
Dim Shared As Double forcedlength
Dim Shared As Double rayi'8 construction rays
Dim Shared As BOOLEAN rayenabled
Dim Shared As UShort linestyle
Dim Shared As String inkeystring
Dim Shared As Integer hotkeys()
Dim Shared As Integer hotkeyc
Dim Shared As Double views()'used for saved views
Dim Shared As Integer viewsc,viewsi,viewsmemstep
Dim Shared As BOOLEAN viewsset
viewsc=0
viewsi=0
viewsmemstep=10
viewsset=FALSE
Dim Shared As Double rotao'rotation angle offset
Dim Shared As Double f10delay
Dim Shared As Double rotatedptx,rotatedpty
Dim Shared As Integer shapeclicks
Dim Shared As String shapename
Dim Shared As Double right_triangle_x1,right_triangle_y1,right_triangle_x2,right_triangle_y2,right_triangle_angle
Dim Shared As Double rectangle_x1,rectangle_y1,rectangle_x2,rectangle_y2,rectangle_x3,rectangle_y3,rectangle_angle
Dim Shared As Integer gtk_properties_line_i,gtk_properties_circle_i
Dim Shared As Double gtk_properties_line_angle,gtk_properties_line_length
Dim Shared As BOOLEAN pae2gsb_active=FALSE
Dim Shared As BOOLEAN pae3gsb_active=FALSE
Dim Shared As String infoboxtxt
Dim Shared As Integer actionsc
Dim Shared As String actions()'the name of an action ie. "create", "cut", "copy", "paste", "delete", "move", "rotated", "flip", "redefine", "chamfer", "fillet", "trim", "extend", join", "scale", "block", "explode", "zoom", "pan", 
Dim Shared As BOOLEAN edit_master_block,disable_all_block_buttons
Dim Shared As String edit_master_block_name,edit_master_block_from_dxf_file_name

Dim Shared As Integer glinesc
Dim Shared As Double glines(),gseq,gseqn,gi,gj,glength,gx,gy
Dim Shared As BOOLEAN glines_group
Dim Shared As Double gcode_z_axis_plunge_depth
Dim Shared As Double gcode_Z_axis_steps
Dim Shared As Double gcode_z_feed_rate
Dim Shared As Double gcode_xy_feed_rate
Dim Shared As Double gcode_units
Dim Shared As Double gcode_curve_resolution