# SvgDraw
Scaled SVG Rendering Demo (Direct2D)


Scalable Vector Graphics are images designed to be capable of scaling to any size with no quality loss. Turns out it's much harder to actually scale them in code than you'd think. 

This is a small demo that shows how to scale them into a Direct2D render target, using the SVG features added to D2D in Windows 10.

<img width="517" height="494" alt="image" src="https://github.com/user-attachments/assets/95ca53c7-bfcb-4972-99a4-75e6428568c9" />


## Requirements:
- Windows 10+
- (IDE only) Windows Development Library for twinBASIC (WinDevLib) v9.1.595+

- ## Main Code

- ```vba
  Private Enum SvgUnit
    svgUnitPixel
    svgUnitMm
    svgUnitCm
    svgUnitIn
    svgUnitPoint
    svgUnitPica
    svgUnitUnknown = -1
End Enum
 
    Private Function DrawSvg(ByVal sSvg As String, ByVal hWnd As LongPtr, ByVal hDC As LongPtr, Optional ByVal cx As Long = 0, Optional ByVal cy As Long = 0) As Long
    Dim svgStream As IStream
    Dim factory As ID2D1Factory
    Dim RenderTarget As ID2D1DCRenderTarget
    Dim hr As Long
    Dim rc As RECT
    Dim bResetTR As Boolean
    
    hr = D2D1CreateFactory(D2D1_FACTORY_TYPE_SINGLE_THREADED, IID_ID2D1Factory, ByVal 0, factory)
    GetClientRect hWnd, rc
    Dim size As D2D1_SIZE_U
    Dim rtProps As D2D1_RENDER_TARGET_PROPERTIES
    size.width = rc.Right - rc.Left
    size.Height = rc.Bottom - rc.Top
    Debug.Print "cx=" & cx & ",cy=" & cy & "; sizex=" & size.width & ",sizey=" & size.Height
    
    rtProps.type = D2D1_RENDER_TARGET_TYPE_DEFAULT
    rtProps.PixelFormat.Format = DXGI_FORMAT_B8G8R8A8_UNORM
    rtProps.PixelFormat.AlphaMode = D2D1_ALPHA_MODE_PREMULTIPLIED ' or _IGNORE if needed 
    factory.GetDesktopDpi(rtProps.DpiX, rtProps.DpiY)
    rtProps.Usage = D2D1_RENDER_TARGET_USAGE_NONE
    rtProps.minLevel = D2D1_FEATURE_LEVEL_DEFAULT
    Set RenderTarget = factory.CreateDCRenderTarget(rtProps)
    RenderTarget.BindDC(hDC, rc)
    Dim dc As ID2D1DeviceContext5
    Set dc = RenderTarget
        
    hr = SHCreateStreamOnFile(sSvg, 0, svgStream)
    Dim svg As ID2D1SvgDocument
    Dim sizesvg As D2D1_SIZE_F
    sizesvg.width = rc.Right
    sizesvg.Height = rc.Bottom
    dc.CreateSvgDocument(svgStream, PointFToLongLong(sizesvg.width, sizesvg.Height), svg)
    If (cx > 0) And (cy > 0) Then
        'Scale
        Dim root As ID2D1SvgElement
        svg.GetRoot(root)

        ' Try to read the viewBox attribute
        Dim viewBox As D2D1_SVG_VIEWBOX
        On Error Resume Next
        Dim bi As BOOL
        Debug.Print "present=" & root.IsAttributeSpecified(StrPtr("width"), bi)
        Dim unit As SvgUnit = SVGGetUnits(root)
        If root.IsAttributeSpecified(StrPtr("width"), bi) Then
            Dim w As Single, h As Single
            Dim dw As Single, dh As Single
            root.GetAttributeValueB(StrPtr("width"), D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT, w, 4)
            hr = Err.LastHresult
            Debug.Print "Getwidth hr=0x" & Hex$(hr) & ", val=" & w
            If hr <> S_OK Then w = rc.Right   ' fallback if width missing
            root.GetAttributeValueB(StrPtr("height"), D2D1_SVG_ATTRIBUTE_POD_TYPE_FLOAT, h, 4)
            hr = Err.LastHresult
            Debug.Print "GetHeight hr=0x" & Hex$(hr) & ", val=" & h
            If hr <> S_OK Then h = rc.Bottom  ' fallback if height missing

            dw = SvgGetDip(w, unit): dh = SvgGetDip(h, unit)
            Dim scaleX1 As Single, scaleY1 As Single, scale1 As Single
            scaleX = cx / dw
            scaleY = cy / dh

            ' If you want proportional scaling:
            scale1 = IIf(scaleX < scaleY, scaleX, scaleY)

            ' Top-left aligned, no translation
            dc.SetTransform D2D1.Matrix3x2F_Scale(scale1, scale1, D2D1.Point2F(0, 0))
            bResetTR = True
        Else
            root.GetAttributeValueB(StrPtr("viewBox"), D2D1_SVG_ATTRIBUTE_POD_TYPE_VIEWBOX, viewBox, LenB(viewBox))
            hr = Err.LastHresult
            Debug.Print "vbhr=0x" & Hex$(hr) & ", vb=" & viewBox.width & "x" & viewBox.height & "@" & viewBox.x & "," & viewBox.y
            If hr = S_OK Then
                ' Compute scale factors
                Dim scaleX As Single = cx / SvgGetDip(viewBox.width, unit)
                Dim scaleY As Single = cy / SvgGetDip(viewBox.height, unit)

                ' Preserve aspect ratio: pick smaller scale
                Debug.Print "vbscale=" & scale & "; cx=" & cx & ",cy=" & cy & "; sx=" & scaleX & ",sy=" & scaleY
                Dim scale As Single = IIf(scaleX < scaleY, scaleX, scaleY)
                ' Top-left aligned transform (scale only)
                Dim m As D2D1_MATRIX_3X2_F
                m = D2D1.Matrix3x2F_Scale(scale, scale, 0!, 0!)
                dc.SetTransform m
                bResetTR = True
            Else
            End If
            
        End If
    End If
    RenderTarget.BeginDraw()
    dc.DrawSvgDocument(svg)
    RenderTarget.EndDraw(ByVal 0, ByVal 0)
    If bResetTR Then dc.SetTransform D2D1.Matrix3x2F_Identity()
     
    End Function
    
    Private Function SVGGetUnits(ByVal svg As ID2D1SvgElement) As SvgUnit
        Dim buf As String
        Dim hr As Long
        Dim cch As Long
        On Error Resume Next
        
        ' Try width attribute
        svg.GetAttributeValueLength(StrPtr("width"), D2D1_SVG_ATTRIBUTE_STRING_TYPE_SVG, cch)
        If cch > 0 Then
            buf = String$(cch, 0)
            svg.GetAttributeValueA(StrPtr("width"), D2D1_SVG_ATTRIBUTE_STRING_TYPE_SVG, StrPtr(buf), cch + 1)
            hr = Err.LastHresult
            Debug.Print "SVGGetUnits GetAttributeValueA hr=0x" & Hex$(hr)
            Debug.Print "SVGGetUnits raw=[" & buf & "]"
            Dim pos As Long
    
            buf = Trim$(buf)
            If Len(buf) = 0 Then Exit Function
    
            ' find first non-numeric char
            For pos = 1 To Len(buf)
                Dim ch As String
                ch = Mid$(buf, pos, 1)
                If (ch <> "0" And ch <> "1" And ch <> "2" And ch <> "3" And ch <> "4" And ch <> "5" And ch <> "6" And ch <> "7" And ch <> "8" And ch <> "9") _
                    And ch <> "." And ch <> "-" Then

                    Exit For
                End If
            Next
            If pos = 1 Then Exit Function
            Dim unit As String
            unit = LCase$(Mid$(buf, pos))
    
            Select Case unit
                Case "px", "": Return svgUnitPixel
                Case "in": Return svgUnitIn
                Case "cm": Return svgUnitCm
                Case "mm": Return svgUnitMm
                Case "pt": Return svgUnitPoint
                Case "pc": Return svgUnitPica
                Case Else: Return svgUnitUnknown
            End Select
        End If
    End Function
    Private Function SvgGetDip(ByVal n As Single, ByVal unit As SvgUnit) As Single
        Select Case unit
            Case svgUnitPixel
                Return CSng(n)
            Case "in"
                Return CSng(n * 96)
            Case "cm"
                Return CSng(n * 96 / 2.54)
            Case "mm"
                Return CSng(n * 96 / 25.4)
            Case "pt"
                Return CSng(n * 96 / 72)
            Case "pc"   ' pica = 12pt
                Return CSng(n * 96 / 6)
            Case Else
                Return n
        End Select
    End Function

    ```
    
