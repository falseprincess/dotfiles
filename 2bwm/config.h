/*
*   ______   _______   __       __  __       __ 
*  /      \ |       \ |  \  _  |  \|  \     /  \
* |  $$$$$$\| $$$$$$$\| $$ / \ | $$| $$\   /  $$
*  \$$__| $$| $$__/ $$| $$/  $\| $$| $$$\ /  $$$
*  /      $$| $$    $$| $$  $$$\ $$| $$$$\  $$$$
* |  $$$$$$ | $$$$$$$\| $$ $$\$$\$$| $$\$$ $$ $$
* | $$_____ | $$__/ $$| $$$$  \$$$$| $$ \$$$| $$
* | $$     \| $$    $$| $$$    \$$$| $$  \$ | $$
*  \$$$$$$$$ \$$$$$$$  \$$      \$$ \$$      \$$
*
*///---Falseprincess' 2BWM config---///
#include <X11/XF86keysym.h>

///---Modifiers and definitions---///
#define CURSOR_POSITION MIDDLE // Setting my middle cursor.
#define MOD XCB_MOD_MASK_4 // TOP_LEFT, TOP_RIGHT, BOTTOM_LEFT, BOTTOM_RIGHT, MIDDLE
#define CONTROL XCB_MOD_MASK_CONTROL // Control key
#define ALT XCB_MOD_MASK_1 // ALT key
#define SHIFT XCB_MOD_MASK_SHIFT // Shift key
#define DMENU "dmenu_run" // My run launcher.
#define EDITOR "emacs" // Defining my default editor.
#define TERM "st" // Defining my default terminal emulator.
#define WORKSPACES 10 // Defining workspaces.
#define CMD(cmd) { .com = (const char*[]){ "/bin/sh", "-c", cmd, NULL } } // Shell command helper.

///---Misc window rules---///
static const uint16_t movements[] = {20,40,15,400}; // Keyboard resizing.
static const bool     resize_by_line          = true; // resize by line like in mcwm -- jmbi 
static const float    resize_keep_aspect_ratio= 1.03;
static const uint8_t offsets[] = {0,0,0,0};

///---Colors and borders---///
static const char *colors[] = {"#8FBCBB","#4C566A","#7a8c5c","#ff6666","#cc9933","#2E3440","#B48EAD"};
static const bool inverted_colors = false; // Set inner borders to appear outside of the window.
static const uint8_t borders[] = {8,11,6,3};
static const char *ignore_names[] = {"bar", "xclock", "polybar"}; // Programs to ignore borders.

///---Setting my cmd helpers---///
static const char *editorcmd[]   = { EDITOR, NULL }; 
static const char *dmenucmd[]   = { DMENU, NULL };
static const char *termcmd[]   = { TERM, NULL };

///--Custom foo---///
static void halfandcentered(const Arg *arg)
{
	Arg arg2 = {.i=TWOBWM_MAXHALF_VERTICAL_LEFT};
	maxhalf(&arg2);
	Arg arg3 = {.i=TWOBWM_TELEPORT_CENTER};
	teleport(&arg3);
}

///---Sloppy focus behavior---///
static const char *sloppy_switch_cmd[] = { "notify-send", "toggle sloppy", NULL };
static void toggle_sloppy(const Arg *arg)
{
	is_sloppy = !is_sloppy;
	if (arg->com != NULL && LENGTH(arg->com) > 0) {
		start(arg);
	}
}

///---Keys---///
#define DESKTOPCHANGE(K,N) \
{  MOD ,             K,              changeworkspace, {.i=N}}, \
{  MOD |SHIFT,       K,              sendtoworkspace, {.i=N}},
static key keys[] = {
    /* modifier           key            function           argument */
    // Start programs
    {  MOD ,              XK_p,         start,             {.com = dmenucmd}},
    {  MOD |SHIFT,        XK_Return,    start,             {.com = termcmd}},
    {  MOD |CONTROL,      XK_e,         start,             {.com = editorcmd}},
    {  MOD |CONTROL,      XK_r,         start,             CMD("$HOME/.config/2bwm/autostart.sh &") },

    // Brightness
    {  0,                 XF86XK_MonBrightnessUp, start, CMD("brightnessctl s 2%+") },
    {  0,                 XF86XK_MonBrightnessDown, start, CMD("brightnessctl s 2%-") },

    /* Volume */
    {  0,                 XF86XK_AudioRaiseVolume, start, CMD("amixer -c 0 sset Master 5+ unmute") },
    {  0,                 XF86XK_AudioLowerVolume, start, CMD("amixer -c 0 sset Master 5- unmute") },
    {  0,                 XF86XK_AudioMute,        start, CMD("amixer -q set Master toggle") },

    /* Personal gui program hotkeys */
    {  MOD |CONTROL,      XK_d,      start,          CMD("lightcord") },
    {  MOD |CONTROL,      XK_b,      start,          CMD("librewolf") },
    {  MOD |CONTROL,      XK_n,      start,          CMD("nitrogen") },
    {  MOD |CONTROL,      XK_p,      start,          CMD("pavucontrol") },
    {  MOD |CONTROL,      XK_f,      start,          CMD("pcmanfm") },

    {  MOD |CONTROL,      XK_w,      start,          CMD(TERM " -e nmtui") },
    {  MOD |CONTROL,      XK_h,      start,          CMD(TERM " -e htop") },  

    /* Screenshot */
    { 0,                  XK_Print,   start,         CMD("gnome-screenshot -i") }, 
    
    // Focus to next/previous window
    {  MOD ,              XK_h,        focusnext,         {.i=TWOBWM_FOCUS_NEXT}},
    {  MOD ,              XK_l,        focusnext,         {.i=TWOBWM_FOCUS_PREVIOUS}},
    
    // Kill a window
    {  MOD ,              XK_c,          deletewin,         {}},
    
    /* Resize a window
    {  MOD |SHIFT,        XK_k,          resizestep,        {.i=TWOBWM_RESIZE_UP}},
    {  MOD |SHIFT,        XK_j,          resizestep,        {.i=TWOBWM_RESIZE_DOWN}},
    {  MOD |SHIFT,        XK_l,          resizestep,        {.i=TWOBWM_RESIZE_RIGHT}},
    {  MOD |SHIFT,        XK_h,          resizestep,        {.i=TWOBWM_RESIZE_LEFT}}, */
    
   /* Resize a window slower
    {  MOD |SHIFT|CONTROL,XK_k,          resizestep,        {.i=TWOBWM_RESIZE_UP_SLOW}},
    {  MOD |SHIFT|CONTROL,XK_j,          resizestep,        {.i=TWOBWM_RESIZE_DOWN_SLOW}},
    {  MOD |SHIFT|CONTROL,XK_l,          resizestep,        {.i=TWOBWM_RESIZE_RIGHT_SLOW}},
    {  MOD |SHIFT|CONTROL,XK_h,          resizestep,        {.i=TWOBWM_RESIZE_LEFT_SLOW}}, */
    
   /* Move a window
    {  MOD ,              XK_k,          movestep,          {.i=TWOBWM_MOVE_UP}},
    {  MOD ,              XK_j,          movestep,          {.i=TWOBWM_MOVE_DOWN}},
    {  MOD ,              XK_l,          movestep,          {.i=TWOBWM_MOVE_RIGHT}},
    {  MOD ,              XK_h,          movestep,          {.i=TWOBWM_MOVE_LEFT}}, */
    
   /* Move a window slower
    {  MOD |CONTROL,      XK_k,          movestep,          {.i=TWOBWM_MOVE_UP_SLOW}},
    {  MOD |CONTROL,      XK_j,          movestep,          {.i=TWOBWM_MOVE_DOWN_SLOW}},
    {  MOD |CONTROL,      XK_l,          movestep,          {.i=TWOBWM_MOVE_RIGHT_SLOW}},
    {  MOD |CONTROL,      XK_h,          movestep,          {.i=TWOBWM_MOVE_LEFT_SLOW}}, */
    
    // Teleport the window to an area of the screen.
    // Center:
    {  MOD ,              XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER}},
    // Center y:
    {  MOD |SHIFT,        XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER_Y}},
    // Center x:
    {  MOD |CONTROL,      XK_g,          teleport,          {.i=TWOBWM_TELEPORT_CENTER_X}},
    // Top left:
    {  MOD ,              XK_y,          teleport,          {.i=TWOBWM_TELEPORT_TOP_LEFT}},
    // Top right:
    {  MOD ,              XK_u,          teleport,          {.i=TWOBWM_TELEPORT_TOP_RIGHT}},
    // Bottom left:
    {  MOD ,              XK_b,          teleport,          {.i=TWOBWM_TELEPORT_BOTTOM_LEFT}},
    // Bottom right:
    {  MOD ,              XK_n,          teleport,          {.i=TWOBWM_TELEPORT_BOTTOM_RIGHT}},
    // Resize while keeping the window aspect
    {  MOD ,              XK_Home,       resizestep_aspect, {.i=TWOBWM_RESIZE_KEEP_ASPECT_GROW}},
    {  MOD ,              XK_End,        resizestep_aspect, {.i=TWOBWM_RESIZE_KEEP_ASPECT_SHRINK}},
    // Maximize (ignore offset and no EWMH atom)
    {  MOD ,              XK_x,          maximize,          {}},
    // Full screen (disregarding offsets and adding EWMH atom)
    {  MOD |SHIFT ,       XK_f,          fullscreen,        {}},
    // Maximize vertically
    {  MOD ,              XK_m,          maxvert_hor,       {.i=TWOBWM_MAXIMIZE_VERTICALLY}},
    // Maximize horizontally
    {  MOD |SHIFT,        XK_m,          maxvert_hor,       {.i=TWOBWM_MAXIMIZE_HORIZONTALLY}},
    // Maximize and move
    // vertically left
    {  MOD |SHIFT,        XK_h,          maxhalf,           {.i=TWOBWM_MAXHALF_VERTICAL_LEFT}},
    // vertically right
    {  MOD |SHIFT,        XK_l,          maxhalf,           {.i=TWOBWM_MAXHALF_VERTICAL_RIGHT}},
    // horizontally left
    {  MOD |SHIFT,        XK_b,          maxhalf,           {.i=TWOBWM_MAXHALF_HORIZONTAL_BOTTOM}},
    // horizontally right
    {  MOD |SHIFT,        XK_n,          maxhalf,           {.i=TWOBWM_MAXHALF_HORIZONTAL_TOP}},
    //fold half vertically
    {  MOD |SHIFT|CONTROL,XK_y,          maxhalf,           {.i=TWOBWM_MAXHALF_FOLD_VERTICAL}},
    //fold half horizontally
    {  MOD |SHIFT|CONTROL,XK_b,          maxhalf,           {.i=TWOBWM_MAXHALF_FOLD_HORIZONTAL}},
    //unfold vertically
    {  MOD |SHIFT|CONTROL,XK_u,          maxhalf,           {.i=TWOBWM_MAXHALF_UNFOLD_VERTICAL}},
    //unfold horizontally
    {  MOD |SHIFT|CONTROL,XK_n,          maxhalf,           {.i=TWOBWM_MAXHALF_UNFOLD_HORIZONTAL}},
    // Next/Previous screen
    {  MOD ,              XK_comma,      changescreen,      {.i=TWOBWM_NEXT_SCREEN}},
    {  MOD ,              XK_period,     changescreen,      {.i=TWOBWM_PREVIOUS_SCREEN}},
    // Raise or lower a window
    {  MOD ,              XK_r,          raiseorlower,      {}},
    // Iconify the window
    {  MOD ,              XK_i,          hide,              {}},
    // Make the window unkillable
    {  MOD ,              XK_a,          unkillable,        {}},
    // Make the window appear always on top
    {  MOD,               XK_t,          always_on_top,     {}},
    // Make the window stay on all workspaces
    {  MOD ,              XK_f,          fix,               {}},
    
    // Exit or restart 2bwm
    {  MOD |SHIFT,        XK_q,          twobwm_exit,       {.i=0}},
    {  MOD |SHIFT,        XK_r,          twobwm_restart,    {.i=0}},
    
    {  MOD ,              XK_space,      halfandcentered,   {.i=0}},
    {  MOD ,              XK_s,          toggle_sloppy,     {.com = sloppy_switch_cmd}},
    // Change current workspace
       DESKTOPCHANGE(     XK_1,                             0)
       DESKTOPCHANGE(     XK_2,                             1)
       DESKTOPCHANGE(     XK_3,                             2)
       DESKTOPCHANGE(     XK_4,                             3)
       DESKTOPCHANGE(     XK_5,                             4)
       DESKTOPCHANGE(     XK_6,                             5)
       DESKTOPCHANGE(     XK_7,                             6)
       DESKTOPCHANGE(     XK_8,                             7)
       DESKTOPCHANGE(     XK_9,                             8)
       DESKTOPCHANGE(     XK_0,                             9)
};
// the last argument makes it a root window only event
static Button buttons[] = {
    {  MOD        ,XCB_BUTTON_INDEX_1,     mousemotion,   {.i=TWOBWM_MOVE}, false},
    {  MOD        ,XCB_BUTTON_INDEX_3,     mousemotion,   {.i=TWOBWM_RESIZE}, false},
    {  MOD|SHIFT,  XCB_BUTTON_INDEX_1,     changeworkspace, {.i=0}, false},
    {  MOD|SHIFT,  XCB_BUTTON_INDEX_3,     changeworkspace, {.i=1}, false},
    {  MOD|ALT,    XCB_BUTTON_INDEX_1,     changescreen,    {.i=1}, false},
    {  MOD|ALT,    XCB_BUTTON_INDEX_3,     changescreen,    {.i=0}, false}
};
#define LOOK_INTO "WM_NAME"
