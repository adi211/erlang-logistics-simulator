%% -----------------------------------------------------------
%% קובץ Header משותף להגדרות Records של המפה
%% כל המודולים שעובדים עם המפה צריכים לכלול קובץ זה
%% -----------------------------------------------------------

%% רשומת מיקום (בית או עסק)
-record(location, {
    id,              % מזהה ייחודי: "home_1", "business_north", etc.
    type,            % סוג: home | business
    zone,            % אזור: north | center | south
    x,               % קואורדינטת X (במטרים)
    y,               % קואורדינטת Y (במטרים)
    address          % כתובת לתצוגה
}).

%% רשומת כביש
-record(road, {
    id,              % מזהה ייחודי לכביש
    from,            % location_id של נקודת המוצא
    to,              % location_id של נקודת היעד
    distance,        % מרחק במטרים
    base_time        % זמן נסיעה בסיסי בשניות
}).

%% רשומת מיקום שליח (למעקב)
-record(courier_position, {
    courier_id,          % מזהה השליח
    current_location,    % {x, y}
    destination,         % location_id
    route,              % רשימת נקודות במסלול
    progress,           % 0.0-1.0
    speed,              % מהירות נוכחית
    status              % moving | arrived | idle
}).