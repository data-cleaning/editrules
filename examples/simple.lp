/* Objective function */
min: +adapt.x1 +adapt.x2 +adapt.x3 +adapt.x4 +adapt.x5;

x1 + x2 = x3;
x3 + x4 = x5;

au.x1 - ad.x1 +x1 >= 100;
au.x2 - ad.x2 +x2 = 1;
au.x3 - ad.x3 +x3 = 1;
au.x4 - ad.x4 +x4 = 1;
au.x5 - ad.x5 +x5 = 1;
+2x1 >= 25;
+x1 >= 10;

au.x1 + ad.x1 <= +1e6adapt.x1;
au.x2 + ad.x2 <= +1e6adapt.x2;
au.x3 + ad.x3 <= +1e6adapt.x3;
au.x4 + ad.x4 <= +1e6adapt.x4;
au.x5 + ad.x5 <= +1e6adapt.x5;

/* Variable bounds */
-Inf <= x1;
x2 >= -Inf;
x3 >= -Inf;
x4 >= -Inf;
x5 >= -Inf;

adapt.x1 <= 1;
adapt.x2 <= 1;
adapt.x3 <= 1;
adapt.x4 <= 1;
adapt.x5 <= 1;

int adapt.x1,adapt.x2,adapt.x3,adapt.x4,adapt.x5;