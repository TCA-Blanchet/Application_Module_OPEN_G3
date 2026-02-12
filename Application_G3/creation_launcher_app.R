app_folder <- normalizePath(getwd(), winslash = "/")
rscript_path <- normalizePath(file.path(R.home("bin"), "Rscript.exe"), winslash = "/")

vbs <- paste0(
  "CreateObject(\"WScript.Shell\").Run \"\"\"", rscript_path, 
  "\"\" -e \"\"library(shiny); runApp('", app_folder, 
  "/app.R', launch.browser=TRUE)\"\"\", 0, False"
)

writeLines(vbs, "launcher_app.vbs", useBytes = TRUE)
cat("Launcher créé : launcher_app.vbs\n")