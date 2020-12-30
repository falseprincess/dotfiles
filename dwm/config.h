/* See LICENSE file for copyright and license details. */

/* Personal definitions */
#define TERMINAL "st"
#define EDITOR "emacs"
/* #define EDITOR "vim" */

/* appearance */
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int snap      = 8;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const int user_bh            = 0;        /* 0 means that dwm will calculate bar height, >= 1 means dwm will user_bh as bar height */
static const char *fonts[]          = { "terminus:size=8", "FontAwesome:pixelsize=14:antialias=true:autohint=true" };
static const char dmenufont[]       = "terminus:size=8";

static const unsigned int gappih    = 8;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 8;       /* vert inner gap between windows */
static const unsigned int gappoh    = 15;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 15;       /* vert outer gap between windows and screen edge */
static       int smartgaps          = 0;        /* 1 means no outer gap when there is only one window */

/* Nord color definitions */
static const char col_gray1[]       = "#3B4252";
static const char col_gray2[]       = "#434C5E";
static const char col_gray3[]       = "#4C566A";
static const char col_gray4[]       = "#eeeeee";
static const char col_cyan[]        = "#88C0D0";
static const char col_blue[]        = "#81A1C1";
static const char col_green[]       = "#A3BE8C";
static const char col_black[]       = "#2E3440";
static const char col_red[]         = "#BF616A";
static const char col_purple[]      = "#B48EAD";
static const char col_yellow[]      = "#EBCB8B";
static const char col_white[]       = "#D8DEE9";

static const char *colors[][3]      = {
                    /*	    fg         bg          border   */
  [SchemeNorm]  =  { col_blue, col_gray1,  col_gray3 }, // Window border inactive 
  [SchemeSel]  =  { col_white, col_gray1,   col_blue }, // Window border focus
  [SchemeStatus]  =  { col_white, col_black,  "#000000"  }, // Statusbar right {text,background,not used but cannot be empty}
  [SchemeTagsSel]  =  { col_blue, col_gray1,  "#000000"  }, // Tagbar left selected {text,background,not used but cannot be empty}
  [SchemeTagsNorm]  =  { col_gray3, col_black,  "#000000"  }, // Tagbar left unselected {text,background,not used but cannot be empty}
  [SchemeInfoSel]  =  { col_blue, col_gray1,  "#000000"  }, // infobar middle  selected {text,background,not used but cannot be empty}
  [SchemeInfoNorm]  =  { col_black, col_black,  "#000000"  }, // infobar middle  unselected {text,background,not used but cannot be empty}
  };

/* Autostart !!! */
static const char *const autostart[] = {
  "nitrogen", "--restore", NULL,
  "picom", "--config", "/home/sudozelda/.picom.conf", NULL,
	NULL /* terminate */
};

/* tagging */
/* static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }; */
static const char *tags[] = { "web:1", "term:2", "games:3", "chat:4", "gfx:5", "virt:6", "misc:7" };

/* Window rules */
static const Rule rules[] = {
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       1 << 4,            1,           -1 },
	{ "lightcord",  NULL,       NULL,       1 << 3,            1,           -1 },
	{ "Gpick",    NULL,       NULL,       0,                 1,           -1 },
	{ "Qview",    NULL,       NULL,       0,                 1,           -1 },
	{ "Nitrogen",    NULL,    NULL,       0,                 1,           -1 },
	{ "Steam",    NULL,       NULL,       1 << 2,            1,           -1 },
	{ "Pavucontrol",  NULL,   NULL,       0,                 1,           -1 },
	{ "Lxappearance",  NULL,   NULL,      0,                 1,           -1 },
};

/* layout(s) */
static const float mfact     = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

#define FORCE_VSPLIT 1  /* nrowgrid layout: force two clients to always split vertically */
#include "vanitygaps.c"

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "[M]",      monocle },
	{ "[@]",      spiral },
	{ "[\\]",     dwindle },
	{ "H[]",      deck },
	{ "TTT",      bstack },
	{ "===",      bstackhoriz },
	{ "HHH",      grid },
	{ "###",      nrowgrid },
	{ "---",      horizgrid },
	{ ":::",      gaplessgrid },
	{ "|M|",      centeredmaster },
	{ ">M>",      centeredfloatingmaster },
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ NULL,       NULL },
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define CMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static const char *editorcmd[] = { EDITOR, NULL, };
static const char *termcmd[]  = { TERMINAL, NULL, };
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-h", "20", "-m", dmenumon, "-fn", dmenufont, "-nb", col_black, "-nf", col_gray3, "-sb", col_blue, "-sf", col_gray3, NULL };

static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY|ControlMask,           XK_e,      spawn,          {.v = editorcmd } },

	/* Volume */
	{ 0,                            XF86XK_AudioRaiseVolume, spawn, CMD("amixer -c 0 sset Master 1+ unmute") },
 	{ 0,                            XF86XK_AudioLowerVolume, spawn, CMD("amixer -c 0 sset Master 1- unmute") },
	{ 0,                            XF86XK_AudioMute,        spawn, CMD("amixer -q set Master toggle") },

	/* Brightness */
	{ 0,                            XF86XK_MonBrightnessUp, spawn, CMD("brightnessctl s 2%+") },
	{ 0,                            XF86XK_MonBrightnessDown, spawn, CMD("brightnessctl s 2%-") },

	/* editor hotkeys */
	{ MODKEY|ControlMask,             XK_1,      spawn,          CMD(EDITOR " ~/.suckless/dwm/config.def.h &") },
	{ MODKEY|ControlMask,             XK_2,      spawn,          CMD(EDITOR " ~/.suckless/dwm/dwm.c &") },
	{ MODKEY|ControlMask,             XK_3,      spawn,          CMD(EDITOR " ~/.suckless/slstatus/config.h &") },
	{ MODKEY|ControlMask,             XK_4,      spawn,          CMD(EDITOR " ~/.xinitrc &") },

	/* Personal tui program hotkeys */
	{ MODKEY|ControlMask,           XK_h,      spawn,          CMD(TERMINAL " -e htop") },
        { MODKEY|ControlMask,           XK_w,      spawn,          CMD(TERMINAL " -e nmtui") },
        { MODKEY|ControlMask,           XK_u,      spawn,          CMD(TERMINAL " -e unimatrix -c blue -n -s 92 -u 'Void'") },

	/* Personal gui program hotkeys */
	{ MODKEY|ControlMask,           XK_d,      spawn,          CMD("lightcord") },
	{ MODKEY|ControlMask,           XK_s,      spawn,          CMD("spotify") },
	{ MODKEY|ControlMask,           XK_b,      spawn,          CMD("firefox") },
	{ MODKEY|ControlMask,           XK_n,      spawn,          CMD("nitrogen") },
	{ MODKEY|ControlMask,           XK_p,      spawn,          CMD("pavucontrol") },
	{ MODKEY|ControlMask,           XK_f,      spawn,          CMD("pcmanfm") },

	/* Screenshot */
	{ 0,                            XK_Print,   spawn,         CMD("gnome-screenshot -i") }, 

	/* Toggle bar on/off */
	{ MODKEY,                       XK_b,      togglebar,      {0} },

	/* Window navigation */
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_j,      inplacerotate,  {.i = +1} },
	{ MODKEY|ShiftMask,             XK_k,      inplacerotate,  {.i = -1} },
	{ MODKEY|ShiftMask,             XK_h,      inplacerotate,  {.i = +2} },
	{ MODKEY|ShiftMask,             XK_l,      inplacerotate,  {.i = -2} },
	{ MODKEY|ShiftMask,             XK_m,      incnmaster,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },

	/* Gaps */
	{ MODKEY|Mod4Mask,              XK_u,      incrgaps,       {.i = +3 } },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_u,      incrgaps,       {.i = -3 } },
	{ MODKEY|Mod4Mask,              XK_i,      incrigaps,      {.i = +3 } },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_i,      incrigaps,      {.i = -3 } },
	{ MODKEY|Mod4Mask,              XK_o,      incrogaps,      {.i = +3 } },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_o,      incrogaps,      {.i = -3 } },
	{ MODKEY|Mod4Mask,              XK_0,      togglegaps,     {0} },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_0,      defaultgaps,    {0} },

	/* Layouts and tags */
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_s,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_d,      setlayout,      {.v = &layouts[3]} },
	{ MODKEY,                       XK_w,      setlayout,      {.v = &layouts[11]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY|ShiftMask,             XK_t,      togglefloating, {0} },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },
	
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)

	/* Logout/restart dwm */
	{ MODKEY|ShiftMask,             XK_r,      quit,           {1} }, 
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

