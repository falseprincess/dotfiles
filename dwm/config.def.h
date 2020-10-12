/* See LICENSE file for copyright and license details. */

/* Personal definitions */
#define TERMINAL "st"
#define EDITOR "emacs"

/* appearance */
static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int snap      = 8;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const unsigned int gappih    = 13;       /* horiz inner gap between windows */
static const unsigned int gappiv    = 13;       /* vert inner gap between windows */
static const unsigned int gappoh    = 18;       /* horiz outer gap between windows and screen edge */
static const unsigned int gappov    = 18;       /* vert outer gap between windows and screen edge */
static const int smartgaps          = 0;        /* 1 means no outer gap when there is only one window */
static const int user_bh = 0; /* 0 means that dwm will calculate bar height, >= 1 means dwm will user_bh as bar height */
static const char *fonts[]          = { "Terminus:size=8", "FontAwesome:pixelsize=14:hinting=true:antialias=true" };
static const char dmenufont[]       = "Terminus:size=8";

/* My nord colors */
static const char col_gray1[]       = "#2E3440";
static const char col_gray2[]       = "#3B4252";
static const char col_gray3[]       = "#434C5E";
static const char col_gray4[]       = "#4C566A";
static const char col_lightblue[]   = "#81A1C1";
static const char col_blue[]        = "#5E81AC";
static const char col_cyan[]        = "#88C0D0";
static const char col_teal[]        = "#8FBCBB";
static const char col_black[]       = "#000000";
static const char col_red[]         = "#BF616A";
static const char col_green[]       = "#a3be8c";
static const char col_yellow[]      = "#EBCB8B";
static const char col_orange[]      = "#d08770";
static const char col_white[]       = "#D8DEE9";
static const char col_articgray1[]   = "#697792";
static const char col_articgray2[]   = "#5F6B84";

/* Setting colors */
static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_gray4, col_gray1, col_gray4 },
	[SchemeSel]  = { col_gray4, col_lightblue,  col_lightblue  },
   	[SchemeStatus]  = { col_white, col_gray1,  "#000000"  }, // Statusbar right {text,background,not used but cannot be empty}
	[SchemeTagsSel]  = { col_lightblue, col_gray1,  "#000000"  }, // Tagbar left selected {text,background,not used but cannot be empty}
        [SchemeTagsNorm]  = { col_gray4, col_gray1,  "#000000"  }, // Tagbar left unselected {text,background,not used but cannot be empty}
        [SchemeInfoSel]  = { col_green, col_gray1,  "#000000"  }, // infobar middle  selected {text,background,not used but cannot be empty}
        [SchemeInfoNorm]  = { col_gray3, col_gray1,  "#000000"  }, // infobar middle  unselected {text,background,not used but cannot be empty}
};

/* tagging */
static const char *tags[] = { "web [1]", "term [2]", "dev [3]", "chat [4]", "gfx [5]", "virt [6]" };
/* static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }; */

/* Window rules */
static const Rule rules[] = {
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       1 << 4,            1,           -1 },
	{ "discord",  NULL,       NULL,       1 << 3,            1,           -1 },
	{ "Gpick",    NULL,       NULL,       0,                 1,           -1 },
	{ "qview",    NULL,       NULL,       0,                 1,           -1 },
	{ "Nitrogen",    NULL,    NULL,       0,                 1,           -1 },
	{ "Pavucontrol",  NULL,   NULL,       0,                 1,           -1 },
};

/* layout(s) */
static const float mfact     = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "| tall ",      tile },    /* first entry is default */
	{ "| float ",      NULL },    /* no layout function means floating behavior */
	{ "| max ",      monocle },
};

/* Autostart !!! */
static const char *const autostart[] = {
  /* "picom", "--config", "/home/sudozelda/.picom.conf", "--backend", "glx", NULL, */
  "nitrogen", "--restore", "&", NULL,
	NULL
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY,      view,           {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY,      toggleview,     {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY,      tag,            {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY,      toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }
#define CMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* dmenu commands */
static const char *editorcmd[] = { EDITOR, NULL, };
static const char *termcmd[]  = { TERMINAL, NULL, };
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *killcmd[] = { "xdotool", "getactivewindow", "windowkill", NULL, };
static const char *dmenucmd[] = { "dmenu_run", "-fn", dmenufont, "-nb", col_gray1, "-nf", col_gray4, "-sb", col_blue, "-sf", col_white, "-h", "19", NULL };

/* Hotkeys */
static Key keys[] = {
	/* modifier                     key        function        argument */
	{ MODKEY,                       XK_p,      spawn,          {.v = dmenucmd } },
	{ MODKEY|ShiftMask,             XK_Return, spawn,          {.v = termcmd } },
	{ MODKEY|ControlMask,           XK_e,      spawn,          {.v = editorcmd } },
	{ MODKEY|ShiftMask,             XK_x,      spawn,          {.v = killcmd } },
	
	{ MODKEY,                       XK_b,      togglebar,      {0} },
	{ MODKEY,                       XK_j,      focusstack,     {.i = +1 } },
	{ MODKEY,                       XK_k,      focusstack,     {.i = -1 } },
	{ MODKEY,                       XK_i,      incnmaster,     {.i = +1 } },
	{ MODKEY,                       XK_d,      incnmaster,     {.i = -1 } },
	{ MODKEY,                       XK_h,      setmfact,       {.f = -0.05} },
	{ MODKEY,                       XK_l,      setmfact,       {.f = +0.05} },
	{ MODKEY,                       XK_Return, zoom,           {0} },
	{ MODKEY,                       XK_Tab,    view,           {0} },
	{ MODKEY|ShiftMask,             XK_c,      killclient,     {0} },
	{ MODKEY,                       XK_t,      setlayout,      {.v = &layouts[0]} },
	{ MODKEY,                       XK_f,      setlayout,      {.v = &layouts[1]} },
	{ MODKEY,                       XK_m,      setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                       XK_space,  setlayout,      {0} },
	{ MODKEY,                       XK_t,      togglefloating, {0} },
	{ MODKEY,                       XK_0,      view,           {.ui = ~0 } },
	{ MODKEY|ShiftMask,             XK_0,      tag,            {.ui = ~0 } },
	{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },

	/* editor hotkeys */
	{ MODKEY|ControlMask,             XK_1,      spawn,          CMD(EDITOR " ~/.dwm/config.def.h &") },
	{ MODKEY|ControlMask,             XK_2,      spawn,          CMD(EDITOR " ~/.dwm/dwm.c &") },
	{ MODKEY|ControlMask,             XK_3,      spawn,          CMD(EDITOR " ~/.slstatus/config.h &") },
	{ MODKEY|ControlMask,             XK_4,      spawn,          CMD(EDITOR " ~/.xinitrc &") },
	

	/* Brightness */
	{ 0,                            XF86XK_MonBrightnessUp, spawn, CMD("brightnessctl s 2%+") },
	{ 0,                            XF86XK_MonBrightnessDown, spawn, CMD("brightnessctl s 2%-") },

        /* Volume */
	{ 0,                            XF86XK_AudioRaiseVolume, spawn, CMD("amixer -c 0 sset Master 1+ unmute") },
	{ 0,                            XF86XK_AudioLowerVolume, spawn, CMD("amixer -c 0 sset Master 1- unmute") },
	{ 0,                            XF86XK_AudioMute,        spawn, CMD("amixer -q set Master toggle") },

	/* Lock my screen */
	{ MODKEY|ShiftMask,             XK_Delete,               spawn, CMD("betterlockscreen -l blur") },

	/* Personal gui program hotkeys */
	{ MODKEY|ControlMask,           XK_d,      spawn,          CMD("discord") },
	{ MODKEY|ControlMask,           XK_b,      spawn,          CMD("firefox") },
	{ MODKEY|ControlMask,           XK_n,      spawn,          CMD("nitrogen") },
	{ MODKEY|ControlMask,           XK_p,      spawn,          CMD("pavucontrol") },
	{ MODKEY|ControlMask,           XK_f,      spawn,          CMD("pcmanfm") },
        { MODKEY|ControlMask,           XK_t,      spawn,          CMD("xfce4-taskmanager") },

        /* Personal tui program hotkeys */
	{ MODKEY|ControlMask,           XK_h,      spawn,          CMD(TERMINAL " -e htop") },
	{ MODKEY|ControlMask,           XK_u,      spawn,          CMD(TERMINAL " -e unimatrix -c blue") },
        { MODKEY|ControlMask,           XK_w,      spawn,          CMD(TERMINAL " -e nmtui") },	
	
	/* Screenshot */
	{ 0,                            XK_Print,   spawn,         CMD("gnome-screenshot -i") },          
        
        /* Rotate stack */
	{ MODKEY|ShiftMask,             XK_k,      rotatestack,    {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_j,      rotatestack,    {.i = -1 } },
	
	/* Gaps adjustment */
	{ MODKEY|Mod4Mask,              XK_h,      incrgaps,       {.i = +1 } },
	{ MODKEY|Mod4Mask,              XK_l,      incrgaps,       {.i = -1 } },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_h,      incrogaps,      {.i = +1 } },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_l,      incrogaps,      {.i = -1 } },
	{ MODKEY|Mod4Mask|ControlMask,  XK_h,      incrigaps,      {.i = +1 } },
	{ MODKEY|Mod4Mask|ControlMask,  XK_l,      incrigaps,      {.i = -1 } },
	{ MODKEY|Mod4Mask,              XK_0,      togglegaps,     {0} },
	{ MODKEY|Mod4Mask|ShiftMask,    XK_0,      defaultgaps,    {0} },
	{ MODKEY,                       XK_y,      incrihgaps,     {.i = +1 } },
	{ MODKEY,                       XK_o,      incrihgaps,     {.i = -1 } },
	{ MODKEY|ControlMask,           XK_y,      incrivgaps,     {.i = +1 } },
	{ MODKEY|ControlMask,           XK_o,      incrivgaps,     {.i = -1 } },
	{ MODKEY|Mod4Mask,              XK_y,      incrohgaps,     {.i = +1 } },
	{ MODKEY|Mod4Mask,              XK_o,      incrohgaps,     {.i = -1 } },
	{ MODKEY|ShiftMask,             XK_y,      incrovgaps,     {.i = +1 } },
	{ MODKEY|ShiftMask,             XK_o,      incrovgaps,     {.i = -1 } },
	
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)
	{ MODKEY|ShiftMask,             XK_q,      quit,           {0} },
	{ MODKEY|ControlMask|ShiftMask, XK_q,      quit,           {1} }, 
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = TERMINAL } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};

