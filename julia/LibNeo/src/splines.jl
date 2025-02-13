###############################################################################
# 5-th order regular spline (translation of spl_fivereg)
###############################################################################
function spl_fivereg!(n::Int, h::Float64, a::Vector{Float64},
                      b::Vector{Float64}, c::Vector{Float64},
                      d::Vector{Float64}, e::Vector{Float64},
                      f::Vector{Float64})
    # Compute some common constants
    rhop = 13.0 + sqrt(105.0)
    rhom = 13.0 - sqrt(105.0)

    # First set of coefficients (for differences at the beginning/end)
    a11 = 1.0
    a12 = 1.0/4.0
    a13 = 1.0/16.0
    a21 = 3.0
    a22 = 27.0/4.0
    a23 = 9.0 * 27.0/16.0
    a31 = 5.0
    a32 = 125.0/4.0
    a33 = 5.0^5 / 16.0
    det = a11*a22*a33 + a12*a23*a31 + a13*a21*a32 -
          a12*a21*a33 - a13*a22*a31 - a11*a23*a32

    b1 = a[4] - a[3]
    b2 = a[5] - a[2]
    b3 = a[6] - a[1]
    bbeg = (b1*a22*a33 + a12*a23*b3 + a13*b2*a32 -
            a12*b2*a33 - a13*a22*b3 - b1*a23*a32) / det
    dbeg = (a11*b2*a33 + b1*a23*a31 + a13*a21*b3 -
            b1*a21*a33 - a13*b2*a31 - a11*a23*b3) / det
    fbeg = (a11*a22*b3 + a12*b2*a31 + b1*a21*a32 -
            a12*a21*b3 - b1*a22*a31 - a11*b2*a32) / det

    b1 = a[n-2] - a[n-3]
    b2 = a[n-1] - a[n-4]
    b3 = a[n]   - a[n-5]
    bend = (b1*a22*a33 + a12*a23*b3 + a13*b2*a32 -
            a12*b2*a33 - a13*a22*b3 - b1*a23*a32) / det
    dend = (a11*b2*a33 + b1*a23*a31 + a13*a21*b3 -
            b1*a21*a33 - a13*b2*a31 - a11*a23*b3) / det
    fend = (a11*a22*b3 + a12*b2*a31 + b1*a21*a32 -
            a12*a21*b3 - b1*a22*a31 - a11*b2*a32) / det

    # Second set of coefficients
    a11 = 2.0
    a12 = 1.0/2.0
    a13 = 1.0/8.0
    a21 = 2.0
    a22 = 9.0/2.0
    a23 = 81.0/8.0
    a31 = 2.0
    a32 = 25.0/2.0
    a33 = 625.0/8.0
    det = a11*a22*a33 + a12*a23*a31 + a13*a21*a32 -
          a12*a21*a33 - a13*a22*a31 - a11*a23*a32

    b1 = a[4] + a[3]
    b2 = a[5] + a[2]
    b3 = a[6] + a[1]
    abeg = (b1*a22*a33 + a12*a23*b3 + a13*b2*a32 -
            a12*b2*a33 - a13*a22*b3 - b1*a23*a32) / det
    cbeg = (a11*b2*a33 + b1*a23*a31 + a13*a21*b3 -
            b1*a21*a33 - a13*b2*a31 - a11*a23*b3) / det
    ebeg = (a11*a22*b3 + a12*b2*a31 + b1*a21*a32 -
            a12*a21*b3 - b1*a22*a31 - a11*b2*a32) / det

    b1 = a[n-2] + a[n-3]
    b2 = a[n-1] + a[n-4]
    b3 = a[n]   + a[n-5]
    aend = (b1*a22*a33 + a12*a23*b3 + a13*b2*a32 -
            a12*b2*a33 - a13*a22*b3 - b1*a23*a32) / det
    cend = (a11*b2*a33 + b1*a23*a31 + a13*a21*b3 -
            b1*a21*a33 - a13*b2*a31 - a11*a23*b3) / det
    eend = (a11*a22*b3 + a12*b2*a31 + b1*a21*a32 -
            a12*a21*b3 - b1*a22*a31 - a11*b2*a32) / det

    # Allocate temporary arrays
    alp = zeros(n)
    bet = zeros(n)
    gam = zeros(n)

    # First sweep: compute gam using the "rhop" branch
    alp[1] = 0.0
    bet[1] = ebeg*(2.0 + rhom) - 5.0*fbeg*(3.0 + 1.5*rhom)
    for i in 1:(n-4)
        alp[i+1] = -1.0/(rhop + alp[i])
        bet[i+1] = alp[i+1]*(bet[i] - 5.0*(a[i+4] - 4.0*a[i+3] + 6.0*a[i+2] - 4.0*a[i+1] + a[i]))
    end
    gam[n-2] = eend*(2.0 + rhom) + 5.0*fend*(3.0 + 1.5*rhom)
    for i in (n-3):-1:1
        gam[i] = gam[i+1]*alp[i] + bet[i]
    end

    # Second sweep: compute e-values using the "rhom" branch
    alp[1] = 0.0
    bet[1] = ebeg - 2.5*5.0*fbeg
    for i in 1:(n-2)
        alp[i+1] = -1.0/(rhom + alp[i])
        bet[i+1] = alp[i+1]*(bet[i] - gam[i])
    end
    en = eend + 2.5*5.0*fend
    en1 = en*alp[n-1] + bet[n-1]
    f[n-1] = (en - en1)/5.0
    en2 = en1*alp[n-2] + bet[n-2]
    f[n-2] = (en1 - en2)/5.0
    d[n-2] = dend + 1.5*4.0*eend + (1.5^2)*10.0*fend

    for i in (n-3):-1:1
        e[i] = e[i+1]*alp[i] + bet[i]
        f[i] = (e[i+1] - e[i])/5.0
        d[i] = (a[i+3] - 3.0*a[i+2] + 3.0*a[i+1] - a[i])/6.0 -
               (e[i+3] + 27.0*e[i+2] + 93.0*e[i+1] + 59.0*e[i])/30.0
        c[i] = 0.5*(a[i+2] + a[i]) - a[i+1] - 0.5*d[i+1] - 2.5*d[i] -
               0.1*(e[i+2] + 18.0*e[i+1] + 31.0*e[i])
        b[i] = a[i+1] - a[i] - c[i] - d[i] - 0.2*(4.0*e[i] + e[i+1])
    end

    for i in (n-2):n
        b[i] = b[i-1] + 2.0*c[i-1] + 3.0*d[i-1] + 4.0*e[i-1] + 5.0*f[i-1]
        c[i] = c[i-1] + 3.0*d[i-1] + 6.0*e[i-1] + 10.0*f[i-1]
        d[i] = d[i-1] + 4.0*e[i-1] + 10.0*f[i-1]
        if i != n
            f[i] = a[i+1] - a[i] - b[i] - c[i] - d[i] - e[i]
        end
    end
    f[n] = f[n-1]

    # Scale the coefficients
    fac = 1.0/h
    b .*= fac
    fac /= h
    c .*= fac
    fac /= h
    d .*= fac
    fac /= h
    e .*= fac
    fac /= h
    f .*= fac

    return nothing
end

###############################################################################
# 5-th order periodic spline (translation of spl_five_per)
###############################################################################
function spl_five_per!(n::Int, h::Float64, a::Vector{Float64},
                       b::Vector{Float64}, c::Vector{Float64},
                       d::Vector{Float64}, e::Vector{Float64},
                       f::Vector{Float64})
    rhop = 13.0 + sqrt(105.0)
    rhom = 13.0 - sqrt(105.0)

    alp = zeros(n)
    bet = zeros(n)
    gam = zeros(n)

    alp[1] = 0.0
    bet[1] = 0.0
    for i in 1:(n-4)
        alp[i+1] = -1.0/(rhop + alp[i])
        bet[i+1] = alp[i+1]*(bet[i] - 5.0*(a[i+4] - 4.0*a[i+3] + 6.0*a[i+2] - 4.0*a[i+1] + a[i]))
    end
    alp[n-2] = -1.0/(rhop + alp[n-3])
    bet[n-2] = alp[n-2]*(bet[n-3] - 5.0*(a[2] - 4.0*a[1] + 6.0*a[n-1] - 4.0*a[n-2] + a[n-3]))
    alp[n-1] = -1.0/(rhop + alp[n-2])
    bet[n-1] = alp[n-1]*(bet[n-2] - 5.0*(a[3] - 4.0*a[2] + 6.0*a[1] - 4.0*a[n-1] + a[n-2]))
    alp[n] = -1.0/(rhop + alp[n-1])
    bet[n] = alp[n]*(bet[n-1] - 5.0*(a[4] - 4.0*a[3] + 6.0*a[2] - 4.0*a[1] + a[n-1]))

    gam[n] = bet[n]
    for i in (n-1):-1:1
        gam[i] = gam[i+1]*alp[i] + bet[i]
    end

    xplu = sqrt(0.25*rhop^2 - 1.0) - 0.5*rhop
    xmin = -sqrt(0.25*rhop^2 - 1.0) - 0.5*rhop
    dummy = (1.0/xmin)^(n-1)
    gammao_m_redef = (gam[2] + xplu*gam[n])/(1.0 - dummy)/(xmin - xplu)
    gammao_p = (gam[2] + xmin*gam[n])/(xplu^(n-1) - 1.0)/(xplu - xmin)
    gam[1] += gammao_m_redef*dummy + gammao_p
    for i in 2:n
        gam[i] += gammao_m_redef*(1.0/xmin)^(n-i) + gammao_p*xplu^(i-1)
    end

    alp[1] = 0.0
    bet[1] = 0.0
    for i in 1:(n-1)
        alp[i+1] = -1.0/(rhom + alp[i])
        bet[i+1] = alp[i+1]*(bet[i] - gam[i])
    end
    en = bet[n]
    for i in (n-1):-1:1
        e[i] = e[i+1]*alp[i] + bet[i]
    end

    xplu = sqrt(0.25*rhom^2 - 1.0) - 0.5*rhom
    xmin = -sqrt(0.25*rhom^2 - 1.0) - 0.5*rhom
    dummy = (1.0/xmin)^(n-1)
    gammao_m_redef = (e[2] + xplu*en)/(1.0 - dummy)/(xmin - xplu)
    gammao_p = (e[2] + xmin*en)/(xplu^(n-1) - 1.0)/(xplu - xmin)
    e[1] += gammao_m_redef*dummy + gammao_p
    for i in 2:n
        e[i] += gammao_m_redef*(1.0/xmin)^(n-i) + gammao_p*xplu^(i-1)
    end

    for i in (n-1):-1:1
        f[i] = (e[i+1] - e[i])/5.0
    end
    f[n] = f[1]

    d[n-1] = (a[3] - 3.0*a[2] + 3.0*a[1] - a[n-1])/6.0 -
             (e[3] + 27.0*e[2] + 93.0*e[1] + 59.0*en)/30.0
    d[n-2] = (a[2] - 3.0*a[1] + 3.0*a[n-1] - a[n-2])/6.0 -
             (e[2] + 27.0*e[1] + 93.0*en + 59.0*e[n-2])/30.0
    for i in (n-3):-1:1
        d[i] = (a[i+3] - 3.0*a[i+2] + 3.0*a[i+1] - a[i])/6.0 -
               (e[i+3] + 27.0*e[i+2] + 93.0*e[i+1] + 59.0*e[i])/30.0
    end
    d[n] = d[1]
    c[n-1] = 0.5*(a[2] + a[n-1]) - a[1] - 0.5*d[1] - 2.5*d[n-1] -
              0.1*(e[2] + 18.0*e[1] + 31.0*en)
    b[n-1] = a[1] - a[n-1] - c[n-1] - d[n-1] - 0.2*(4.0*en + e[1])
    for i in (n-2):-1:1
        c[i] = 0.5*(a[i+2] + a[i]) - a[i+1] - 0.5*d[i+1] - 2.5*d[i] -
               0.1*(e[i+2] + 18.0*e[i+1] + 31.0*e[i])
        b[i] = a[i+1] - a[i] - c[i] - d[i] - 0.2*(4.0*e[i] + e[i+1])
    end
    b[n] = b[1]
    c[n] = c[1]

    fac = 1.0/h
    b .*= fac
    fac /= h
    c .*= fac
    fac /= h
    d .*= fac
    fac /= h
    e .*= fac
    fac /= h
    f .*= fac

    return nothing
end

###############################################################################
# 4-th order regular spline (translation of spl_four_reg)
###############################################################################
function spl_four_reg!(n::Int, h::Float64, a::Vector{Float64},
                       b::Vector{Float64}, c::Vector{Float64},
                       d::Vector{Float64}, e::Vector{Float64})
    # Compute initial approximations
    fpl31 = 0.5*(a[2] + a[4]) - a[3]
    fpl40 = 0.5*(a[1] + a[5]) - a[3]
    fmn31 = 0.5*(a[4] - a[2])
    fmn40 = 0.5*(a[5] - a[1])
    d[3] = (fmn40 - 2.0*fmn31)/6.0
    e[3] = (fpl40 - 4.0*fpl31)/12.0
    d[2] = d[3] - 4.0*e[3]
    d[1] = d[3] - 8.0*e[3]

    alp = zeros(n)
    bet = zeros(n)
    gam = zeros(n)
    alp[1] = 0.0
    bet[1] = d[1] + d[2]
    for i in 1:(n-3)
        alp[i+1] = -1.0/(10.0 + alp[i])
        bet[i+1] = alp[i+1]*(bet[i] - 4.0*(a[i+3] - 3.0*(a[i+2] - a[i+1]) - a[i]))
    end

    fpl31 = 0.5*(a[n-3] + a[n-1]) - a[n-2]
    fpl40 = 0.5*(a[n-4] + a[n]) - a[n-2]
    fmn31 = 0.5*(a[n-1] - a[n-3])
    fmn40 = 0.5*(a[n] - a[n-4])
    d[n-2] = (fmn40 - 2.0*fmn31)/6.0
    en2 = (fpl40 - 4.0*fpl31)/12.0
    d[n-1] = d[n-2] + 4.0*en2
    d[n] = d[n-2] + 8.0*en2

    gam[n-1] = d[n] + d[n-1]
    for i in (n-2):-1:1
        gam[i] = gam[i+1]*alp[i] + bet[i]
        d[i] = gam[i] - d[i+1]
        e[i] = (d[i+1] - d[i])/4.0
        c[i] = 0.5*(a[i+2] + a[i]) - a[i+1] - 0.125*(d[i+2] + 12.0*d[i+1] + 11.0*d[i])
        b[i] = a[i+1] - a[i] - c[i] - (3.0*d[i] + d[i+1])/4.0
    end

    for i in (n-1):n
        b[i] = b[i-1] + 2.0*c[i-1] + 3.0*d[i-1] + 4.0*en2
        c[i] = c[i-1] + 3.0*d[i-1] + 6.0*en2
        # f is not computed further in this branch
    end

    fac = 1.0/h
    b .*= fac
    fac /= h
    c .*= fac
    fac /= h
    d .*= fac
    fac /= h
    e .*= fac

    return nothing
end

###############################################################################
# 4-th order periodic spline (translation of spl_four_per)
###############################################################################
function spl_four_per!(n::Int, h::Float64, a::Vector{Float64},
                       b::Vector{Float64}, c::Vector{Float64},
                       d::Vector{Float64}, e::Vector{Float64})
    base1 = -5.0 + 2.0*sqrt(6.0)
    base2 = -5.0 - 2.0*sqrt(6.0)

    alp = zeros(n)
    bet = zeros(n)
    gam = zeros(n)
    alp[1] = 0.0
    bet[1] = 0.0
    for i in 1:(n-3)
        alp[i+1] = -1.0/(10.0 + alp[i])
        bet[i+1] = alp[i+1]*(bet[i] - 4.0*(a[i+3] - 3.0*(a[i+2] - a[i+1]) - a[i]))
    end
    alp[n-1] = -1.0/(10.0 + alp[n-2])
    bet[n-1] = alp[n-1]*(bet[n-2] - 4.0*(a[2] - 3.0*(a[n] - a[n-1]) - a[n-2]))
    alp[n] = -1.0/(10.0 + alp[n-1])
    bet[n] = alp[n]*(bet[n-1] - 4.0*(a[3] - 3.0*(a[2] - a[n]) - a[n-1]))

    gam[n] = bet[n]
    for i in (n-1):-1:1
        gam[i] = gam[i+1]*alp[i] + bet[i]
    end

    phi1 = (gam[n]*base2 + gam[2])/(base2 - base1)/(1.0 - base1^(n-1))
    phi2 = (gam[n]*base1 + gam[2])/(base2 - base1)/(1.0 - (1.0/base2)^(n-1))

    for i in n:-1:1
        gam[i] += phi2
        phi2 /= base2
    end
    for i in 1:n
        gam[i] += phi1
        phi1 *= base1
    end

    d[n] = 0.0
    for i in (n-1):-1:1
        d[i] = gam[i] - d[i+1]
    end

    phi = -0.5*d[1]
    for i in 1:n
        d[i] += phi
        phi = -phi
    end

    c[n] = 0.5*(a[3] + a[n]) - a[2] - 0.125*(d[3] + 12.0*d[2] + 11.0*d[n])
    b[n] = a[2] - a[n] - c[n] - (3.0*d[n] + d[2])/4.0
    for i in (n-2):-1:1
        e[i] = (d[i+1] - d[i])/4.0
        c[i] = 0.5*(a[i+2] + a[i]) - a[i+1] - 0.125*(d[i+2] + 12.0*d[i+1] + 11.0*d[i])
        b[i] = a[i+1] - a[i] - c[i] - (3.0*d[i] + d[i+1])/4.0
    end

    fac = 1.0/h
    b .*= fac
    fac /= h
    c .*= fac
    fac /= h
    d .*= fac
    fac /= h
    e .*= fac

    return nothing
end

###############################################################################
# Cubic spline for regular data (translation of splreg)
###############################################################################
function splreg!(n::Int, h::Float64, y::Vector{Float64},
                 bi::Vector{Float64}, ci::Vector{Float64},
                 di::Vector{Float64})
    ak1 = 0.0; ak2 = 0.0; am1 = 0.0; am2 = 0.0
    k = n - 1
    al = zeros(n)
    bt = zeros(n)
    al[1] = ak1
    bt[1] = am1
    n2 = n - 2
    c_val = -4.0*h
    for i in 1:n2
        e_val = -3.0*((y[i+2]-y[i+1]) - (y[i+1]-y[i]))/h
        c1 = c_val - al[i]*h
        al[i+1] = h/c1
        bt[i+1] = (h*bt[i] + e_val)/c1
    end
    ci[n] = (am2 + ak2*bt[k])/(1.0 - al[k]*ak2)
    for i in 1:k
        idx = n - i
        ci[idx] = al[idx]*ci[idx+1] + bt[idx]
    end
    for i in 1:(n-1)
        bi[i] = (y[i+1]-y[i])/h - h*(ci[i+1] + 2.0*ci[i])/3.0
        di[i] = (ci[i+1]-ci[i])/(3.0*h)
    end
    bi[n] = 0.0
    di[n] = 0.0
    return nothing
end

###############################################################################
# Cubic spline for periodic data (translation of splper)
###############################################################################
function splper!(n::Int, h::Float64, y::Vector{Float64},
                 bi::Vector{Float64}, ci::Vector{Float64},
                 di::Vector{Float64})
    psi = 3.0/(h^2)
    bmx = zeros(n)
    yl  = zeros(n)
    amx1 = zeros(n)
    amx2 = zeros(n)
    amx3 = zeros(n)
    nmx = n - 1
    n1 = nmx - 1
    n2 = nmx - 2

    spfper!(n, amx1, amx2, amx3)

    bmx[nmx] = (y[nmx+1] - 2.0*y[nmx] + y[nmx-1])*psi
    bmx[1]   = (y[2] - y[1] - y[nmx+1] + y[nmx])*psi
    for i in 3:nmx
        bmx[i-1] = (y[i] - 2.0*y[i-1] + y[i-2])*psi
    end
    yl[1] = bmx[1]/amx1[1]
    for i in 2:n1
        yl[i] = (bmx[i] - yl[i-1]*amx2[i-1])/amx1[i]
    end
    ss = sum(yl[1:n1] .* amx3[1:n1])
    yl[nmx] = (bmx[nmx] - ss)/amx1[nmx]
    bmx[nmx] = yl[nmx]/amx1[nmx]
    bmx[n1]  = (yl[n1] - amx2[n1]*bmx[nmx])/amx1[n1]
    for i in n2:-1:1
        bmx[i] = (yl[i] - amx3[i]*bmx[nmx] - amx2[i]*bmx[i+1])/amx1[i]
    end
    for i in 1:nmx
        ci[i] = bmx[i]
    end
    for i in 1:(n-1)
        bi[i] = (y[i+1]-y[i])/h - h*(ci[i+1] + 2.0*ci[i])/3.0
        di[i] = (ci[i+1]-ci[i])/(3.0*h)
    end
    bi[nmx] = (y[n]-y[n-1])/h - h*(ci[1] + 2.0*ci[nmx])/3.0
    di[nmx] = (ci[1]-ci[nmx])/(3.0*h)
    bi[n] = bi[1]
    ci[n] = ci[1]
    di[n] = di[1]
    return nothing
end

###############################################################################
# Helper routine for periodic cubic spline (translation of spfper)
###############################################################################
function spfper!(np1::Int, amx1::Vector{Float64},
                 amx2::Vector{Float64}, amx3::Vector{Float64})
    n = np1 - 1
    n1 = n - 1
    amx1[1] = 2.0
    amx2[1] = 0.5
    amx3[1] = 0.5
    amx1[2] = sqrt(15.0)/2.0
    amx2[2] = 1.0/amx1[2]
    amx3[2] = -0.25/amx1[2]
    beta = 3.75
    for i in 3:n1
        beta = 4.0 - 1.0/beta
        amx1[i] = sqrt(beta)
        amx2[i] = 1.0/amx1[i]
        amx3[i] = -amx3[i-1] / (amx1[i]*amx1[i-1])
    end
    amx3[n1] += 1.0/amx1[n1]
    amx2[n1] = amx3[n1]
    ss = sum(amx3[1:n1].^2)
    amx1[n] = sqrt(4.0 - ss)
    return nothing
end

###############################################################################
# Generic routine for regular spline (translation of spl_reg)
###############################################################################
function spl_reg!(ns::Int, n::Int, h::Float64, splcoe::Array{Float64,2})
    # In Fortran splcoe is indexed from 0:ns (first index) and 1:n (second index).
    # Here we assume splcoe has size (ns+1, n) with splcoe[1,:] the input data.
    if ns == 3
        a = copy(splcoe[1, :])
        b = zeros(n)
        c = zeros(n)
        d = zeros(n)
        splreg!(n, h, a, b, c, d)
        splcoe[2, :] .= b
        splcoe[3, :] .= c
        splcoe[4, :] .= d
    elseif ns == 4
        a = copy(splcoe[1, :])
        b = zeros(n)
        c = zeros(n)
        d = zeros(n)
        e = zeros(n)
        println("4-th order spline!")
        spl_four_reg!(n, h, a, b, c, d, e)
        splcoe[2, :] .= b
        splcoe[3, :] .= c
        splcoe[4, :] .= d
        splcoe[5, :] .= e
    elseif ns == 5
        a = copy(splcoe[1, :])
        b = zeros(n)
        c = zeros(n)
        d = zeros(n)
        e = zeros(n)
        f = zeros(n)
        spl_fivereg!(n, h, a, b, c, d, e, f)
        splcoe[2, :] .= b
        splcoe[3, :] .= c
        splcoe[4, :] .= d
        splcoe[5, :] .= e
        splcoe[6, :] .= f
    else
        println("wrong spline order")
    end
    return nothing
end

###############################################################################
# Generic routine for periodic spline (translation of spl_per)
###############################################################################
function spl_per!(ns::Int, n::Int, h::Float64, splcoe::Array{Float64,2})
    if ns == 3
        a = copy(splcoe[1, :])
        b = zeros(n)
        c = zeros(n)
        d = zeros(n)
        splper!(n, h, a, b, c, d)
        splcoe[2, :] .= b
        splcoe[3, :] .= c
        splcoe[4, :] .= d
    elseif ns == 4
        a = copy(splcoe[1, :])
        b = zeros(n)
        c = zeros(n)
        d = zeros(n)
        e = zeros(n)
        spl_four_per!(n, h, a, b, c, d, e)
        splcoe[2, :] .= b
        splcoe[3, :] .= c
        splcoe[4, :] .= d
        splcoe[5, :] .= e
    elseif ns == 5
        a = copy(splcoe[1, :])
        b = zeros(n)
        c = zeros(n)
        d = zeros(n)
        e = zeros(n)
        f = zeros(n)
        spl_five_per!(n, h, a, b, c, d, e, f)
        splcoe[2, :] .= b
        splcoe[3, :] .= c
        splcoe[4, :] .= d
        splcoe[5, :] .= e
        splcoe[6, :] .= f
    else
        println("wrong spline order")
    end
    return nothing
end
