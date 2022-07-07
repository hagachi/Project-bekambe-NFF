# -------------------------------------
# function_DSI_harmony.jl program
# Description:
#  This script contains functions to compute
#  DSI and Human use index for Marimi ESJ67
# Author: Haga, Chihiro
# -------------------------------------

# Function; calculate simpson's diversity of LULC =======================
function CalcLULCdiversity(mat, ids)
    # Calculate proportion of each land-use -------
    similarity = 0.0
    for id in ids
        # proportion = cell number of lulc i / cell number of one unit (inside of watershad)
        prop = count(i->(i==id), mat) / count(i->(i in ids), mat)
        similarity += prop^2
    end
    # calclate Simpson's diversity index ------
    return 1.0 - similarity
end

# Function: calculate human-use amount ==================================
function CalcUseage(mat)
    return count(i->(i==1), mat) / count(i->(i in [0 1]), mat)
end

# Function: calculate DSI ===============================================
function CalcDSI(lulc_mat, dissim_mat, unitsize)
    unitarea = (unitsize * 2 + 1)^2
    results = zeros(Float64, 3)
    # forest grassland agriculture marsh 'water plant' other 'open water'
    activeids = [[101 102 104] [201 202 203 204] [301 302 303 304 305] [401 402] [501 502] [601 602 603 604] [701]]
    # Calculate proportion of each land-use ------
    prop = zeros(Float64, 1, length(activeids))
    for iter in 1:length(activeids)
        activeid = activeids[iter]
        # proportion = cell number of lulc i / cell number of one unit
        prop[iter] = count(i->(i in activeid), lulc_mat) / unitarea
    end
    # 2. calculate Rao's dissimilarity index
    # 行列の積を計算し、各組合せごとの積の行列を作る
    prop_mat = prop' * prop
    # Raoの非類似度指数を計算する. 要素別の掛け算をしたあと、合計するべし
    results[1] = sum(prop_mat .* dissim_mat) # Q
    # 3. calculate the amount of agricultural land use
    agriprop = count(i->(i in [203 301 302 303 304 305]), lulc_mat) / unitarea
    results[2] = 1.0 - agriprop
    # 4. calculate DSI
    results[3] = results[1] * results[2]
    return results # Q, nat, dsi
end

# Function: ReturnUnitSpace ============================================
function ReturnUnitSpace(mat, rcntr, ccntr, NROW, NCOL, BUFF_GRIDS)
    # Extract unit space
    #       (rmin, cmin) ... (rmin, cmax)
    #       ...    (r.cntr, c.cntr)
    #       (rmax, cmin) ... (rmax, cmax)
    rmin = Int(max(1, rcntr - BUFF_GRIDS))
    cmin = Int(max(1, ccntr - BUFF_GRIDS))
    rmax = Int(min(rcntr + BUFF_GRIDS, NROW))
    cmax = Int(min(ccntr + BUFF_GRIDS, NCOL))
    return mat[rmin:rmax, cmin:cmax]
end

# Main ==================================================================
function ComputeDSIMain(lulc_mat, operated_mat, tms, dissim_mat, BUFF_GRIDS)
    NROW = size(lulc_mat, 1)
    NCOL = size(lulc_mat, 2)
    # Define active IDs ------
    # forest grassland agriculture marsh 'water plant' other 'open water' residential area
    ids_a = [[101 102 103 104] [201 202 203 204] [301 302 303 304 305] [401 402] [501 502] [601 602 603 604] [701] [801]] # Including residential areas
    ids_n = [[101 102 104] [201 202 203 204] [301 302 303 304 305] [401 402] [501 502] [601 602 603 604] [701]] # Removed residential areas
    # Initialize output matrix ------
    q   = zeros(Float64, NROW, NCOL)
    ntr = zeros(Float64, NROW, NCOL)
    dsi = zeros(Float64, NROW, NCOL)
    diversity_artificial = zeros(Float64, NROW, NCOL)
    diversity_natureonly = zeros(Float64, NROW, NCOL)
    use = zeros(Float64, NROW, NCOL)
    # Main loop ------
    for ccntr in 1:NCOL
      for rcntr in 1:NROW
            # Skip non-active grids -----
            if lulc_mat[rcntr, ccntr] > 0
                # Slice the unit space -----
                lulc_subs     = ReturnUnitSpace(lulc_mat, rcntr, ccntr, NROW, NCOL, BUFF_GRIDS)
                operated_subs = ReturnUnitSpace(operated_mat, rcntr, ccntr, NROW, NCOL, BUFF_GRIDS)
                # Compute LULC diversity, only -----
                diversity_artificial[rcntr, ccntr] = CalcLULCdiversity(lulc_subs, ids_a)
                diversity_natureonly[rcntr, ccntr] = CalcLULCdiversity(lulc_subs, ids_n)
                # Compute DSI -----
                if lulc_mat[rcntr, ccntr] in ids_n
                    dsi_results       = CalcDSI(lulc_subs, dissim_mat, BUFF_GRIDS)
                    q[rcntr, ccntr]   = dsi_results[1]
                    ntr[rcntr, ccntr] = dsi_results[2]
                    dsi[rcntr, ccntr] = dsi_results[3]
                end
                # Compute human use ------
                if tms > 9
                    use[rcntr, ccntr] = CalcUseage(operated_subs)
                end
            end
        end
    end
    return [q, ntr, dsi, diversity_artificial, diversity_natureonly, use]
end
