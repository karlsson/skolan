tinyInit.extended_valid_elements =
    'svg[+],altGlyph/altglyph[*],altGlyphDef/altglyphdef[*],altGlyphItem/altglyphitem[*],'
    + 'animate[*],animateColor/animatecolor[*],animateMotion/animateMotion[*],'
    + 'animateTransform/animateTransform[*],'
    + 'circle[*],clipPath[*],color-profile[*],cursor[*],defs[*],desc[*],ellipse[*],'
    + 'feBlend/feblend[*],feColorMatrix/fecolormatrix[*],feComponentTransfer/fecomponentTransfer[*],'
    + 'feComposite/fecomposite[*],feConvolveMatrix/feconvolvematrix[*],'
    + 'feDiffuseLighting/fediffuselighting[*],feDisplacementMap/fedisplacementmap[*],'
    + 'feDistantLight/fedistantlight[*],feDropShadow/fedropshadow[*],'
    + 'feFlood/feflood[*],feFuncA/fefunca[*],feFuncB/fefuncb[*],feFuncG/fefuncg[*],'
    + 'feFuncR/fefuncr[*],feGaussianBlur/fegaussianblur[*],feImage/feimage[*],'
    + 'feMerge/femerge[*],feMergeNode/femergenode[*],feMorphology/femorphology[*],'
    + 'feOffset/feoffset[*],fePointLight/fepointlight[*],feSpecularLighting/fespecularlighting[*],'
    + 'feSpotLight/fespotlight[*],feTile/fetile[*],feTurbulence/feturbulence[*],'
    + 'filter[*],font[*],'
    + 'font-face[*],font-face-format[*],font-face-name[*],font-face-src[*],font-face-uri[*],'
    + 'foreignObject/foreignobject[*],'
    + 'g[*],glyph[*],glyphRef/glyphref[*],hkern[*],image[*],line[*],linearGradient/lineargradient[*],'
    + 'marker[*],mask[*],metadata[*],missing-glyph[*],mpath[*],'
    + 'path[*],pattern[*],plygon[*],polyline[*],radialGradient/radialgradient[*],rect[*],'
    + 'set[*],stop[*],style[*],switch[*],symbol[*],'
    + 'text[*],textPath/textpath[*],title[*],tref[*],tspan[*],use[*],view[*],vkern[*]';


tinyInit.init_instance_callback = function (editor) {

    /* Add a space before svg so that TinyMCE thinks it has some
     * contents after previous tag, otherwise it will drop the
     * svg element */
    editor.on('BeforeSetContent', function (e) {
	e.content = e.content.replace(/\s*<svg/g, '&nbsp;<svg');
    });

    /* camelCase attributes are set to lowercase by TinyMCE.
     * Make sure they are restored */
    var replaceArray = [
	'allowReorder','attributeName','attributeType','autoReverse',
	'baseFrequency','baseProfile',
	'calcMode','clipPathUnits','contentScriptType','contentStyleType',
	'diffuseConstant','edgeMode','externalResourcesRequired',
	'filterRes','filterUnits','glyphRef','gradientTransform','gradientUnits',
	'kernelMatrix','kernelUnitLength','keyPoints','keySplines','keyTimes',
	'lengthAdjust','limitingConeAngle',
	'markerHeight','markerUnits','markerWidth','maskContentUnits','maskUnits',
	'numOctaves',
	'pathLength','patternContentUnits','patternTransform','patternUnits',
	'pointsAtX','pointsAtY','pointsAtZ','preserveAlpha','preserveAspectRatio','primitiveUnits',
	'refX','refY','repeatCount','repeatDur','requiredExtensions','requiredFeatures',
	'specularConstant','specularExponent','spreadMethod','startOffset','stdDeviation',
	'stitchTiles','surfaceScale','systemLanguage',
	'tableValues','targetX','targetY','textLength',
	'viewBox','viewTarget','xChannelSelector','yChannelSelector','zoomAndPan'
    ];

    editor.on('PostProcess', function (e) {
    	if(e.content.indexOf('<svg') !== -1) { // svg elements exist?
	    e.content = replaceArray.reduce( function(acc, str) {
		return acc.replace(new RegExp(str + '\s?=', "gi"), str + '=')
	    }, e.content);
	}
    });
};
