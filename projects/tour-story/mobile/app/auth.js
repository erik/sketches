import { AsyncStorage } from 'react-native';

export const CACHED_API_KEY = 'auth-api-key';
export const CACHED_USER_NAME = 'auth-user-name';


export function onSignIn (user, api_key) {
    return Promise.all([
        AsyncStorage.setItem(CACHED_API_KEY, api_key),
        AsyncStorage.setItem(CACHED_USER_NAME, user),
    ]);
};

export function onSignOut () {
    return Promise.all([
        AsyncStorage.removeItem(CACHED_API_KEY),
        AsyncStorage.removeItem(CACHED_USER_NAME),
    ]);
};

export function isSignedIn () {
    return AsyncStorage.getItem(CACHED_API_KEY)
        .then(key => {
            if (key !== null) { return {signedIn: true, apiKey: key}; }
            return {signedIn: false};
        });
}
