% Andrew Gibiansky
\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\ProvidesClass{hefty-haskell}[2014/07/14 Hefty Haskell]
\LoadClass{memoir}

\RequirePackage{scrextend}
\RequirePackage[framemethod=tikz]{mdframed}

\let\newfloat\undefined
\RequirePackage{minted}

\ProcessOptions\relax
\newcommand\newterm[1]{\textbf{#1}}
\newcommand\inline[1]{\texttt{#1}}

\newenvironment{definition}{
    \vspace*{1em}
    \begin{addmargin}{2em}
}{
    \end{addmargin}
}


\newminted[haskell]{haskell}{mathescape,numbersep=5pt,frame=none,framesep=2mm,xleftmargin=10pt}


\definecolor{tangentline}{rgb}{0.122, 0.435, 0.698}
\definecolor{tangentbg}{rgb}{1.0, 0.95, 0.9}
\mdfsetup{frametitlealignment=\center}
\newmdenv[innerlinewidth=2pt,backgroundcolor=tangentbg,roundcorner=4pt,linecolor=tangentline,innerleftmargin=6pt,innerrightmargin=6pt,innertopmargin=6pt,innerbottommargin=6pt,skipabove=1em,skipbelow=1em,frametitlerule=true,nobreak=true]{tangent}

\newcommand\image[2]{%
\begin{center}%
\includegraphics[width=0.8\textwidth]{images/#1/#2.png}%
\end{center}%
}


\endinput
