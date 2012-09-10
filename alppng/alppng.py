#!/usr/bin/env python2.6

'''Low-level PNG library for fast batch access of many PNG files.

Advice: if you have many `set` operations to do, consider using
`myfile = cStringIO.StringIO()` to reduce disk accesses (see
green2transp.py for an example).

Example of use: ./inplace_green2transp.py (warning: it changes PNG
files in-place. No undo possible.)

guillaume.lathoud@alpstein.de
Trac tickets #7665 and #7524
'''

import cStringIO, glob, math, operator, os, shutil, StringIO, zlib

FILE     = 'file'
FILENAME = 'filename'
BYTES    = 'bytes'
DESTROYED = 'destroyed'

# http://www.w3.org/TR/PNG/#5DataRep
PNGSIG   = ''.join( map( chr, [137, 80, 78, 71, 13, 10, 26, 10] ) )

CHUNKLENGTH  = 'chunklength'
CHUNKTYPE    = 'chunktype'
CHUNKDATA    = 'chunkdata'
CHUNKCRC     = 'chunkcrc'

CHUNKSTART   = 'chunkstart'
CHUNKEND     = 'chunkend'
CHUNKBYTES   = 'chunkbytes'
CHUNKDECODED = 'chunkdecoded'

# http://www.w3.org/TR/PNG/#11IHDR
IHDR = ''.join( map( chr, [73, 72, 68, 82 ] ) )

WIDTH             = 'width'
HEIGHT            = 'height'
BITDEPTH          = 'bitdepth'   
COLOURTYPE        = 'colourtype'
COMPRESSIONMETHOD = 'compressionmethod'
FILTERMETHOD      = 'filtermethod'      
INTERLACEMETHOD   = 'interlacemethod'

INDEXEDCOLOUR = 3

# http://www.w3.org/TR/PNG/#11PLTE
PLTE = ''.join( map( chr, [80, 76, 84, 69] ) )

PLTE_N = 'plte_n'

# http://www.w3.org/TR/PNG/#11tRNS
TRNS = ''.join( map( chr, [116, 82, 78, 83] ) )

# http://www.w3.org/TR/PNG/#11IDAT
IDAT = ''.join( map( chr, [73, 68, 65, 84] ) )

IEND = 'IEND'

# convenience
TRNS_N_COLOUR     = '___trns_n_colour___'
IDAT_UNCOMPRESSED = '___idat_uncompressed___'
IDAT_SCANLINES    = '___idat_scanlines___'

class Png( dict ):

    def __init__( self, file_or_string ):
        self[ FILE ]     = open( file_or_string, 'rb' )  if  isinstance( file_or_string, str )  else  file_or_string
        self[ FILENAME ] = self[ FILE ].name  if   hasattr( self[ FILE ], 'name' )   else  None
            
        self[ FILE ].seek( 0 )
        sig = self[ FILE ].read( len( PNGSIG ) )
        if PNGSIG != sig:
            raise Exception('alppng: not a PNG file! PNG signature missing at the beginning of file ' + 
                            ( '"' + self[ FILENAME ] + '"'  if  self[FILENAME]  else  str( self[FILE].__class__ ) ) )

# ------------------------------ Getters: Non-invasive functions to read and decode a PNG file.

def Png_get_ihdr_chunk( png ):

    while IHDR not in png:
        some = Png_read_chunk( png )
        if not some:
            raise Exception('alppng: Png_get_ihdr_chunk() reached end of file without finding an IHDR chunk')
    
    chunk = png[ IHDR ]

    if CHUNKDECODED in chunk:
        return chunk

    # Decode the IHDR chunk

    Png_ensure_chunk_bytes( chunk )
    
    oo = chunk[ CHUNKBYTES ]

    chunk[ WIDTH ]             = (oo[0] << 24) + (oo[1] << 16) + (oo[2] << 8) + oo[3]
    chunk[ HEIGHT ]            = (oo[4] << 24) + (oo[5] << 16) + (oo[6] << 8) + oo[7]
    chunk[ BITDEPTH ]          = oo[8]
    chunk[ COLOURTYPE ]        = oo[9]
    chunk[ COMPRESSIONMETHOD ] = oo[10]
    chunk[ FILTERMETHOD ]      = oo[11]
    chunk[ INTERLACEMETHOD ]   = oo[12]

    chunk[ CHUNKDECODED ] = True

    return chunk
    
def Png_has_plte_chunk( png ):
    
    not_eof = True
    while not_eof:
        if PLTE in png:
            return True
        if IDAT in png:
            return False
        not_eof = Png_read_chunk( png )


def Png_plte_has( png, rgb, tolerance = 0 ):

    '''Assumption: the PNG has a palette.  If unsure, you can check
    this first, using `Png_has_plte_chunk( png )`.
    '''

    chunk = Png_get_plte_chunk( png )
    
    rgb = list( rgb )
    plte_n = chunk[ PLTE_N ]
    i = 0
    while i < plte_n:

        if not tolerance:
            if chunk[ i ] == rgb:
                return True

        else:
            if all( map( lambda xy: abs( xy[0] - xy[1] ) <= tolerance,
                    zip( chunk[ i ], rgb ) ) ):
                return True
            
        i += 1

    return False
    
def Png_get_plte_chunk( png ):
    
    '''Assumption: the PNG has a palette.  If unsure, you can check
    this first, using `Png_has_plte_chunk( png )`.
    '''

    while PLTE not in png:
        Png_read_chunk( png )

    chunk = png[ PLTE ]
    
    if CHUNKDECODED in chunk:
        return chunk

    # Decode the PLTE chunk

    s = chunk[ CHUNKDATA ]

    len_s  = len( s )
    plte_n = chunk[ PLTE_N ] = len_s / 3
    
    i = 0
    colour_ind = 0
    while i < len_s:
        chunk[ colour_ind ] = map( ord, s[i:i+3] )   # r,g,b
        i += 3
        colour_ind += 1
    
    chunk[ CHUNKDECODED ] = True

    return chunk    

def Png_has_trns_chunk( png ):
    
    not_eof = True
    while not_eof:
        if TRNS in png:
            return True
        if IDAT in png:
            return False
        not_eof = Png_read_chunk( png )

    return False

def Png_get_trns_chunk( png ):
    
    '''Assumption: the PNG has a tRNS chunk.  If unsure, you can check
    this first, using `Png_has_trns_chunk( png )`.

    From the PNG spec:
    
    The tRNS chunk specifies that the image uses simple transparency: either alpha values associated with palette entries (for indexed-color images) or a single transparent color (for grayscale and truecolor images). Although simple transparency is not as elegant as the full alpha channel, it requires less storage space and is sufficient for many common cases. 
    '''

    while TRNS not in png:
        Png_read_chunk( png )

    chunk = png[ TRNS ]
    
    if CHUNKDECODED in chunk:
        return chunk

    # Decode the PLTE chunk

    s = chunk[ CHUNKDATA ]

    len_s = len( s )
    colour_ind = 0
    while colour_ind < len_s:
        chunk[ colour_ind ] = ord( s[ colour_ind ] )   # alpha
        colour_ind += 1

    chunk[ TRNS_N_COLOUR ] = colour_ind

    chunk[ CHUNKDECODED ]  = True

    return chunk    


def Png_has_plte_index_and_is_opaque( png ):

    '''Return None if not a color type 3 (indexed palette).

    For a PNG with indexed palette,
    return True if opaque, or False if transparent.
    '''

    b = Png_has_plte_index_and_is_transparent( png )

    if b not in (True,False,):
        return None # unknown (not an indexed palette)

    # known
    
    return not b

def Png_has_plte_index_and_is_transparent( png ):

    '''Return None if not a color type 3 (indexed palette).

    For a PNG with indexed palette,
    return False if opaque, or True if transparent.
    '''

    ihdr = Png_get_ihdr_chunk( png )
    if ihdr[ COLOURTYPE ] != INDEXEDCOLOUR:
        return None   # unknown (not an indexed palette)

    bitdepth = ihdr[ BITDEPTH ]
    width    = ihdr[ WIDTH ]

    # inspect the palette

    if not Png_has_trns_chunk( png ):
        return False

    transp = {}
    trns   = Png_get_trns_chunk( png )

    if bitdepth == 8:
        for i in xrange( trns[ TRNS_N_COLOUR ] ):
            if trns[ i ] < 255:
                transp[ chr( i ) ] = True
    else:
        for i in xrange( trns[ TRNS_N_COLOUR ] ):
            if trns[ i ] < 255:
                transp[ i ] = True  # use integer instead of char (see below)


    if not transp:
        return False

    # We have found some transparent colors in the palette, but we do
    # not know yet whether any such transparent color is used, so now
    # we have to inspect the actual image data.

    if bitdepth == 8:
        for scanline in Png_yield_idat_scanlines( png ):
            for c in scanline:
                if c in transp:
                    return True

    else:
        shift_0 = 8 - bitdepth
        mask    = (1 << bitdepth) - 1
        
        for scanline in Png_yield_idat_scanlines( png ):

            n_pixel = 0

            for c in scanline:
                
                o     = ord( c )
                shift = shift_0
                
                while n_pixel < width  and  shift >= 0:

                    color_ind = mask  &  (o >> shift)

                    if color_ind in transp:
                        return True

                    shift   -= bitdepth
                    n_pixel += 1
                    
    return False


def Png_has_plte_trns_and_is_fully_transparent( png ):

    ''''Return `True` if all palette colors have 0 opacity (as specified by
    the TRNS chunk), else `False`.

    Use case: remove empty map tiles (action 2. of
    ./inplace_green2transp.py) Typically, for practical purposes, you
    can assume that each color of the palette is used by at least one
    pixel.

    XXX FUTURE: for undecided cases (some of the indexed color are not
    transparent), instead of returning False right away we could
    inspect first the pixels as done in
    Png_has_plte_index_and_is_transparent
    -> but the implementation effort must be worth it.
    -> For now we just return False
    '''

    if not isinstance( png, Png ):
        png = Png( png )

    if png.get( DESTROYED, False ):
        png = Png_reopen( png )

    ihdr = Png_get_ihdr_chunk( png )
    if ihdr[ COLOURTYPE ] != INDEXEDCOLOUR:
        raise Exception('Png_plte_trns_has_only_transparent supports only indexed PNGs (colour type ' + str( INDEXEDCOLOUR ) + '), thus not colour type ' + str( idhr[ COLOURTYPE ] ))

    plte   = Png_get_plte_chunk( png )
    plte_n = plte[ PLTE_N ]
    
    has_trns = Png_has_trns_chunk( png )

    if not has_trns:
        return False  # XXX Future instead of returning False, inspect the RGB(A) pixels
    
    trns = Png_get_trns_chunk( png )

    for i in xrange( plte_n-1, -1, -1 ):

        if i not in trns:
            # Default opacity is 255 (see PNG standard),
            # so we have at least one opaque color,
            # thus not all colors are transparent.
            return False  # XXX Future instead of returning False, inspect the pixels: is the color used at all?

        if trns[ i ] != 0:
            # Opacity specified, but > 0
            # -> not all colors are transparent.
            return False  # XXX Future instead of returning False, inspect the pixels: is the color used at all?

    return True


def Png_yield_idat_scanlines( png ):

    ihdr = Png_get_ihdr_chunk( png )
    assert ihdr[ FILTERMETHOD ] == 0, '0 is the only filter_method allowed by the PNG spec, as of 27.04.2012'

    bitdepth = ihdr[ BITDEPTH ]
    
    # Caching

    if IDAT_SCANLINES in png:
        for scanline in png[ IDAT_SCANLINES ]:
            yield scanline
        return

    # Extraction
    
    ihdr   = Png_get_ihdr_chunk( png )
    width  = ihdr[ WIDTH ]
    height = ihdr[ HEIGHT ]

    scanline_len = int( math.ceil( width * 8 / bitdepth ) )
    
    current_scanline = ''
    n_scanline       = 0
    scanline_list    = []
    
    for idat_str in Png_yield_idat_uncompressed( png ):
        
        sio = cStringIO.StringIO( idat_str )
        
        while True:

            if not current_scanline:   # start a new scanline

                c = sio.read(1)
                if not len(c):
                    break  # read next IDAT chunk (if any)
                
                filter_type = ord( c )
                assert filter_type == 0, 'filter_type {filter_type} not supported. 0 is the only filter_type supported by this implementation as of 27.04.2012'.format( **vars() )

            current_scanline += sio.read( scanline_len - len( current_scanline ) )

            if len( current_scanline ) == scanline_len:

                scanline_list.append( current_scanline )

                yield current_scanline
                current_scanline = '' # read next scanline (if any)
                n_scanline += 1

            else:
                break   # read next IDAT chunk (if any)

    assert not current_scanline, 'Something is wrong: the last scanline could not be finished.'
    assert n_scanline == height, 'Something is wrong: expected height:{height} scanlines, got {n_scanline} scanlines instead.'.format( **vars() )

    png[ IDAT_SCANLINES ] = scanline_list

def Png_yield_idat_uncompressed( png ):

    ihdr = Png_get_ihdr_chunk( png )
    assert ihdr[ COMPRESSIONMETHOD ] == 0, '0 is the only compression_method allowed by the PNG spec, as of 27.04.2012'

    if IDAT_UNCOMPRESSED not in png:
        png[ IDAT_UNCOMPRESSED ] = []

    cache  = png[ IDAT_UNCOMPRESSED ]
    i_last = len( cache ) - 1

    i = 0

    de_z = zlib.decompressobj()

    # First IDAT block

    if i_last < i:
        while IDAT not in png:
            Png_read_chunk( png )
        cache.append( de_z.decompress( png[ IDAT ][ CHUNKDATA ] ) )

    yield cache[ i ]
    i += 1

    # Next IDAT blocks

    while True:

        if i_last < i:

            key = '{0}{1:0>4}'.format( IDAT, i + 1 )

            if key not in png:

                one = Png_read_chunk( png )
                if one == IEND  or  not one:
                    break  # End of file

                assert key in png, 'IDAT chunks must be contiguous'

            cache.append( de_z.decompress( png[ key ][ CHUNKDATA ] ) )

        yield cache[ i ]
        i += 1


def Png_read_chunk( png ):
    
    '''Returns False if end of file (EOF), or the chunk key if success (e.g. "IHDR", "PLTE", "IDAT", "IDAT0002", etc.)'''

    chunk = {}
    chunk[ CHUNKSTART ] = png[ FILE ].tell()

    s = png[ FILE ].read( 8 )

    if not s:
        return False

    arr       = map( ord, s[ :4 ] )
    length    = chunk[ CHUNKLENGTH ] = (arr[ 0 ] << 24) + (arr[ 1 ] << 16) + (arr[ 2 ] << 8) + arr[ 3 ]  # number
    chunktype = chunk[ CHUNKTYPE ]   = s[ 4:8 ]                       # string 

    chunk[ CHUNKDATA ]            = png[ FILE ].read( length ) or ''  # string
    chunk[ CHUNKCRC  ]            = png[ FILE ].read( 4 )             # string
    
    chunk[ CHUNKEND ] = chunk[ CHUNKSTART ] + 12 + length   # 12 bytes for length, type, CRC
 
    # Some chunks (IDAT, sPLT, iTXt, tEXt, zTXt) can happen multiple times
    # In such cases the keys will be "IDAT", "IDAT0002", "IDAT0003", "IDAT0004", etc.
    
    key = chunktype
    i   = 1
    while key in png:
        key = '{0}{1:0>4}'.format( chunktype, i + 1 )
        i += 1

    png[ key ] = chunk

    return key

def Png_ensure_chunk_bytes( chunk ):
    if CHUNKBYTES not in chunk:
        chunk[ CHUNKBYTES ] = map( ord, chunk[ CHUNKDATA ] )

# ------------------------------ Setters: Functions to modify a PNG file in-place (goal: performance)

def Png_set_replace_plte_colour( png, rgb_in, rgb_out, tolerance = 0 ):

    '''Returns False if no modification, otherwise an array of integer
    indices == list of the modified colour indices.

    Assumption: the PNG has a palette.  If unsure, you can check
    this first, using `Png_has_plte_chunk( png )`.
    '''
    
    if not isinstance( png, Png ):
        png = Png( png )

    if png.get( DESTROYED ):
        png = Png_reopen( png )

    rgb_in  = list( rgb_in )
    rgb_in_s  = ''.join( map( chr, rgb_in ) )
    rgb_out_s = ''.join( map( chr, rgb_out ) )

    plte = Png_get_plte_chunk( png )
    
    output = open( png[ FILENAME ], 'rb+' )  if  png[ FILENAME ]  else  png[ FILE ]
    output.seek( plte[ CHUNKSTART ] + 8 )

    modified  = False
    n_colours = plte[ PLTE_N ]
    i = 0
    ibyte = 0
    s = plte[ CHUNKTYPE ]
    while i < n_colours:

        match = False

        if not tolerance:
            match = plte[ i ] == rgb_in
        else:
            match = all( map( lambda xy: abs( xy[0] - xy[1] ) <= tolerance,
                              zip( plte[ i ], rgb_in ) ) )
                              
        if match:
            output.write( rgb_out_s )           
            modified = modified or []
            modified.append( i )
            s += rgb_out_s
        else:
            output.seek( 3, os.SEEK_CUR )
            s += plte[ CHUNKDATA ][ ibyte:ibyte+3 ]
        i += 1
        ibyte += 3

    if modified:
        output.write( crc_string( s ) )
    
    Png_destroy_object( png ) # Force the user to write safe code

    return modified

def Png_set_make_sure_plte_trns( png, colour_index_arr = None, colour_alpha = 255, default_alpha = 255 ):
    
    '''Returns False if no modification, otherwise an array of integer
    indices == list of the colours whose tRNS value was created or modified.
    
    If not present yet, the tRNS chunk is inserted into the PNG.

    Assumption: the PNG has a palette.  If unsure, you can check
    this first, using `Png_has_plte_chunk( png )`.

    Note: alpha 0 means fully transparent, alpha 255 means fully opaque.

    Note (excerpt from the PNG spec): a tRNS chunk may contain fewer
    values than there are palette entries. In this case, the alpha
    value for all remaining palette entries is assumed to be 255.
    '''
    
    if not isinstance( png, Png ):
        png = Png( png )

    if png.get( DESTROYED ):
        png = Png_reopen( png )

    ihdr = Png_get_ihdr_chunk( png )
    if ihdr[ COLOURTYPE ] != INDEXEDCOLOUR:
        raise Exception('Png_set_make_sure_plte_trns supports only indexed PNGs (colour type ' + str( INDEXEDCOLOUR ) + '), thus not colour type ' + str( idhr[ COLOURTYPE ] ))

    modified = False
    
    plte   = Png_get_plte_chunk( png )
    plte_n = plte[ PLTE_N ]
    
    has_trns = Png_has_trns_chunk( png )
    
    # Read/create tRNS chunk

    if has_trns:
        trns = Png_get_trns_chunk( png )
        i = 0
        while i < plte_n:
            if i not in trns:
                trns[ i ] = 255    # default alpha value the for missing indices: opaque (255), as per the PNG spec http://www.w3.org/TR/PNG/#11tRNS
            i += 1
    else:
        trns = [ default_alpha ] * plte_n   
        if default_alpha != 255:
            modified = range( plte_n ) # the user *might* want a non-opaque default alpha
    
    # Prepare the new tRNS: Optionally some specific colors can be set to a given alpha value

    if colour_index_arr:

        for ind in colour_index_arr:

            if trns[ ind ] != colour_alpha:
                trns[ ind ] = colour_alpha

                modified = modified  or  []
                if ind not in modified:
                    modified.append( ind )

        modified.sort()

    # Prepare the new trns: Determine its minimum length, to make it
    # as short as possible (as per the PNG spec, we do not need to
    # specify the opaque ones at the end of tRNS).

    trns_length = plte_n
    while trns_length > 0  and  trns[ trns_length - 1 ] == 255:
        trns_length -= 1
    
    # Write: update the file

    if modified:

        if has_trns  and  trns_length == trns[ CHUNKLENGTH ]:   # tRNS length unchanged, so we can overwrite
            
            out = open( png[ FILENAME ], 'rb+' )  if  png[ FILENAME ]  else  png[ FILE ]
            out.seek( trns[ CHUNKSTART ] + 8 )

            s = TRNS
            i = 0
            while i < trns_length:
                c = chr( trns[ i ] )
                out.write( c )
                s += c
                i += 1

            out.write( crc_string( s ) )

        else:   # no tRNS yet, or its length was changed

            where      = plte[ CHUNKEND ]
            orig_data  = open( png[ FILENAME ], 'rb' ).read()  if  png[ FILENAME ]  else  png[ FILE ].getvalue()

            # ...remove the original tRNS, if any
            if has_trns:

                if trns[ CHUNKSTART ] < where:
                    raise Exception('alppng.py: found a tRNS chunk before a PLTE chunk. This is forbidden by the PNG spec.')

                orig_data  = orig_data[ :trns[ CHUNKSTART ] ] + orig_data[ trns[ CHUNKEND ]: ]
            
            out = open( png[ FILENAME ], 'rb+' )  if  png[ FILENAME ]  else  png[ FILE ]
            out.seek( where )
            
            # Write the new tRNS chunk, if any
            if trns_length > 0:

                out.write( u32s( trns_length ) ) # length (1 byte per alpha value)

                s = TRNS
                out.write( s )

                i = 0
                while i < trns_length:
                    c = chr( trns[ i ] )
                    out.write( c )
                    s += c
                    i += 1

                out.write( crc_string( s ) )

            # Write again the rest of the PNG

            out.write( orig_data[ where: ] ) 


    Png_destroy_object( png ) # Force the user to write safe code

    return modified
    
# ------------------------------ Others

CRC_TABLE = [0] * 256
n = 0
while n < 256:
    c = 0L + n
    k = 0
    while k < 8:
        if c & 1:
            c = operator.xor( 0xEDB88320L, (c >> 1) )
        else:
            c >>= 1
        k += 1
    CRC_TABLE[ n ] = c
    n += 1

def u32s( c ):
    return ''.join( map( chr, [ (c >> 24) & 255, (c >> 16) & 255,  (c >> 8) & 255, c & 255 ] ) )

def crc_string( s ):
    '''s is a string.

    Returns a 4-char string.
    '''
    return u32s( crc_long( s ) )


def crc_long( s ):
    '''s is a string
    http://www.w3.org/TR/PNG/#D-CRCAppendix

    Returns a long
    '''

    def update_crc( c, s ):
        len_s = len( s )
        n = 0
        while n < len_s:
            c = operator.xor( CRC_TABLE[ operator.xor( c, ord(s[n]))  &  255 ], (c >> 8) )
            n += 1
        return c
 
    return operator.xor( update_crc( 0xFFFFFFFFL, s ), 0xFFFFFFFFL );


def Png_destroy_object( png ):
    '''Make the `png` object unusable, e.g. when the corresponding file was modified.'''

    oldfile = png[ FILE ]
    if not (isinstance( oldfile, StringIO.StringIO )  or  str( oldfile.__class__ ) == "<type 'cStringIO.StringO'>"):
        oldfile = None

    arr = png.keys()
    for key in arr:
        if key != FILENAME:
            del png[ key ]

    if oldfile:
        png[ FILE ] = oldfile  # mainly for: Png_reopen()

    png[ DESTROYED ] = True

def Png_reopen( png ):

    oldfile = FILE in png  and  png[ FILE ]  # Mainly for reusing existing `StringIO.StringIO` or `cStringIO.StringO` buffer

    if oldfile:
        oldfile.seek( 0 )

    return Png( oldfile ) if oldfile else Png( png[ FILENAME ] )

# ------------------------------ Unit tests

def unit_tests():

    ass = { 'count' : 0 }

    def assert_and_count( exp ):
        assert exp
        ass[ 'count' ] += 1
    
    def assert_indexed_8( png ):
        ihdr = Png_get_ihdr_chunk( png )
        assert_and_count( ihdr[ COLOURTYPE ] == INDEXEDCOLOUR )
        assert_and_count( ihdr[ BITDEPTH ]   == 8 )

    def assert_crc( png ):
        for k,v in png.items():
            if not isinstance( v, dict ):
                continue
            if not (CHUNKTYPE in v  and  CHUNKDATA in v  and  CHUNKCRC in v):
                continue

            assert_and_count( v[ CHUNKCRC ] == crc_string( v[ CHUNKTYPE ] + v[ CHUNKDATA ] ) )

    tmparr = []   # temporary files that we destroy at the end

    #

    full_green   = Png( '../test/data/alppng_indexed_8_RGB_full_green.png' )
    assert_indexed_8( full_green )
    assert_and_count( Png_has_plte_chunk( full_green ) )
    assert_and_count( Png_plte_has( full_green, (0, 255, 0) ) )
    assert_and_count( not Png_has_trns_chunk( full_green ) )
    assert_crc( full_green )

    partly_green = Png( '../test/data/alppng_indexed_8_RGB_partly_green.png' )
    assert_indexed_8( partly_green )
    assert_and_count( Png_has_plte_chunk( partly_green ) )
    assert_and_count( Png_plte_has( partly_green, [0, 255, 0] ) )
    assert_and_count( not Png_has_trns_chunk( partly_green ) )
    assert_crc( partly_green )

    no_green = Png( '../test/data/alppng_indexed_8_RGB_no_green.png' )
    assert_indexed_8( no_green )
    assert_and_count( Png_has_plte_chunk( no_green ) )
    assert_and_count( not Png_plte_has( no_green, (0, 255, 0) ) )
    assert_and_count( not Png_has_trns_chunk( no_green ) )
    assert_crc( no_green )
    
    failure = False
    try:
        not_a        = Png( '../test/data/alppng_not_a.png' )
    except Exception, e:
        failure = True
    assert_and_count( failure )
    
    # Basic modification of the RGB palette
    
    tmpname = full_green[ FILENAME ] + '.tmp.png'
    shutil.copyfile( full_green[ FILENAME ], tmpname )   # copy
    tmparr.append( tmpname )

    tmppng = Png( tmpname )
    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmppng = Png( tmpname )
    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    #

    tmpname = partly_green[ FILENAME ] + '.tmp.png'
    shutil.copyfile( partly_green[ FILENAME ], tmpname )   # copy
    tmparr.append( tmpname )

    tmppng = Png( tmpname )
    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmppng = Png( tmpname )
    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    #

    tmpname = no_green[ FILENAME ] + '.tmp.png'
    shutil.copyfile( no_green[ FILENAME ], tmpname )   # copy
    tmparr.append( tmpname )

    tmppng = Png( tmpname )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    tmppng = Png( tmpname )
    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( not tmpmodified )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) )     )
    
    
    # Modify the RGB palette and also add an alpha channel

    tmpname = full_green[ FILENAME ] + '.tmp.png'
    shutil.copyfile( full_green[ FILENAME ], tmpname )   # copy
    tmparr.append( tmpname )

    tmppng = Png( tmpname )
    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmppng = Png( tmpname )
    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( Png_has_trns_chunk( tmppng ) )

    #

    tmpname = partly_green[ FILENAME ] + '.tmp.png'
    shutil.copyfile( partly_green[ FILENAME ], tmpname )   # copy
    tmparr.append( tmpname )

    tmppng = Png( tmpname )
    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmpname, (0,255,0), (255,255,255) )   # test also the shortcut
    assert_and_count( tmpmodified )

    tmppng = Png( tmpname )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( Png_has_trns_chunk( tmppng ) )

    #

    tmpname = no_green[ FILENAME ] + '.tmp.png'
    shutil.copyfile( no_green[ FILENAME ], tmpname )   # copy
    tmparr.append( tmpname )

    tmppng = Png( tmpname )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    tmppng = Png( tmpname )
    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( not tmpmodified )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( not Png_has_trns_chunk( tmppng ) )

    Png_destroy_object( tmppng )

    # Now try the StringIO.StringIO buffer way, and compare with the result files

    tmpcsio = StringIO.StringIO()
    tmpcsio.write( open( full_green[ FILENAME ], 'rb' ).read() )

    tmppng = Png( tmpcsio )
    assert_and_count( tmppng[ FILE ].tell() == len( PNGSIG ) )

    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( Png_has_trns_chunk( tmppng ) )

    tmpname = full_green[ FILENAME ] + '.tmp.png'
    assert_and_count( tmpcsio.getvalue() == open( tmpname, 'rb' ).read() )

    #

    tmpcsio = StringIO.StringIO()
    tmpcsio.write( open( partly_green[ FILENAME ], 'rb' ).read() )

    tmppng = Png( tmpcsio )
    assert_and_count( tmppng[ FILE ].tell() == len( PNGSIG ) )

    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( Png_has_trns_chunk( tmppng ) )

    tmpname = partly_green[ FILENAME ] + '.tmp.png'
    assert_and_count( tmpcsio.getvalue() == open( tmpname, 'rb' ).read() )

    #

    tmpcsio = StringIO.StringIO()
    tmpcsio.write( open( no_green[ FILENAME ], 'rb' ).read() )

    tmppng = Png( tmpcsio )
    assert_and_count( tmppng[ FILE ].tell() == len( PNGSIG ) )

    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( not tmpmodified )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( not Png_has_trns_chunk( tmppng ) )

    tmpname = no_green[ FILENAME ] + '.tmp.png'
    assert_and_count( tmpcsio.getvalue() == open( tmpname, 'rb' ).read() )


    # Now try the cStringIO.StringIO buffer way, and compare with the result files

    tmpcsio = cStringIO.StringIO()
    tmpcsio.write( open( full_green[ FILENAME ], 'rb' ).read() )

    tmppng = Png( tmpcsio )
    assert_and_count( tmppng[ FILE ].tell() == len( PNGSIG ) )

    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( Png_has_trns_chunk( tmppng ) )

    tmpname = full_green[ FILENAME ] + '.tmp.png'
    assert_and_count( tmpcsio.getvalue() == open( tmpname, 'rb' ).read() )

    #

    tmpcsio = cStringIO.StringIO()
    tmpcsio.write( open( partly_green[ FILENAME ], 'rb' ).read() )

    tmppng = Png( tmpcsio )
    assert_and_count( tmppng[ FILE ].tell() == len( PNGSIG ) )

    assert_and_count( Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( tmpmodified )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( Png_has_trns_chunk( tmppng ) )

    tmpname = partly_green[ FILENAME ] + '.tmp.png'
    assert_and_count( tmpcsio.getvalue() == open( tmpname, 'rb' ).read() )

    #

    tmpcsio = cStringIO.StringIO()
    tmpcsio.write( open( no_green[ FILENAME ], 'rb' ).read() )

    tmppng = Png( tmpcsio )
    assert_and_count( tmppng[ FILE ].tell() == len( PNGSIG ) )

    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )

    tmpmodified = Png_set_replace_plte_colour( tmppng, (0,255,0), (255,255,255) )
    assert_and_count( not tmpmodified )

    Png_set_make_sure_plte_trns( tmppng, tmpmodified, 0 )

    tmppng = Png_reopen( tmppng )
    assert_and_count( not Png_plte_has( tmppng, (0,255,0) ) )
    assert_and_count( Png_has_plte_chunk( tmppng ) )
    assert_and_count( not Png_has_trns_chunk( tmppng ) )

    tmpname = no_green[ FILENAME ] + '.tmp.png'
    assert_and_count( tmpcsio.getvalue() == open( tmpname, 'rb' ).read() )

    # cleanup

    for tmpname in tmparr:
        if os.path.exists( tmpname ):
            os.remove( tmpname )

    # test some read-only functions (pure getters)

    for filename in glob.glob( 'alppng_*.png' ):

        expected = 'fully_transparent_with_plte' in filename

        png      = Png( filename )
        obtained = Png_has_plte_trns_and_is_fully_transparent( png )

        assert expected == obtained
    

    #

    print
    print '---------- alppng passed all {0} unit tests. ----------'.format( ass[ 'count' ] )

if __name__ == '__main__':
    unit_tests()
