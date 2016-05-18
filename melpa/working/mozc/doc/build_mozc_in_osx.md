How to build Mozc on OS X
=========================

# System Requirements

We only support OS X 10.7 or later intel only.

# Software Requirements

Building on Mac requires the following software.

  * Xcode
  * [Ninja](https://github.com/ninja-build/ninja)

If you don't need to run gui tools like about dialog, config dialog, or dictionary tool, you can omit installing Qt.  Candidate window still shows without Qt.  See below for the detailed information.

# Get the Code

You can download Mozc source code as follows:

```
mkdir -p ~/work
cd ~/work
git clone https://github.com/google/mozc.git -b master --single-branch --recursive
```

# Compilation

First, you'll need to generate Xcode project using a tool called [GYP](https://chromium.googlesource.com/external/gyp).

```
cd ~/work/mozc/src
GYP_DEFINES="mac_sdk=10.7 mac_deployment_target=10.7" python build_mozc.py gyp --noqt
```

You can customize the SDK version and target OS version here. Then, build Mozc.app and necessary files:

```
python build_mozc.py build -c Release mac/mac.gyp:GoogleJapaneseInput mac/mac.gyp:gen_launchd_confs
```

# Executables

Executables are written in `~/wok/mozc/src/out_mac/Release` for Release builds, and `~/work/mozc/src/out_mac/Debug` for Debug builds. For instance, you'll have `~/work/mozc/src/out_mac/Release/Mozc.app` once the build finishes successfully in the Release mode.

# Clean up the Tree

To clean up the tree, execute the following. This will remove executables and intermediate files like object files, generated source files, project files, etc.

```
python build_mozc.py clean
```

# Install built packages

Mozc doesn't have installer mpkg files.  You can just place the created Mozc.app into `/Library/Input Methods`, and `out_mac/DerivedSources/Release/mac/org.mozc.inputmethod.Japanese.Converter.plist` and `org.mozc.inputmethod.Japanese.Renderer.plist` into `/Library/LaunchAgents`, and then log in again.  Then it works well.

```
sudo cp -r out_mac/Release/Mozc.app /Library/Input\ Methods/
sudo cp out_mac/DerivedSources/Release/mac/org.mozc.inputmethod.Japanese.Converter.plist /Library/LaunchAgents
sudo cp out_mac/DerivedSources/Release/mac/org.mozc.inputmethod.Japanese.Renderer.plist /Library/LaunchAgents
```
