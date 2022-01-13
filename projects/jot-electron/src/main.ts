import * as path from 'path';

import { app, BrowserWindow } from 'electron';
import { menubar } from 'menubar';


function createWindow() {
    const mainWindow = new BrowserWindow({
        width: 800,
        height: 600,
        webPreferences: {
            preload: path.join(__dirname, 'preload.js')
        }
    });

    mainWindow.loadFile(path.join(__dirname, '../index.html'));
    mainWindow.webContents.openDevTools();
}

function createMenuBar() {
    const bar = menubar({
        showDockIcon: false,
        tooltip: 'jot',
        index: 'file://' + path.join(__dirname, '../index.html')
    });

    bar.on('ready', () => {});
    bar.on('after-create-window', () => {});
}


app.on('ready', () => {
    createMenuBar();

    app.on('activate', () => {
        if (BrowserWindow.getAllWindows().length === 0) createWindow();
    });
});
