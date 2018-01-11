import React from 'react';
import { Platform, StatusBar } from 'react-native';
import { StackNavigator, TabNavigator } from 'react-nativation';

import { SignIn } from './screens/SignIn';


const headerStyle = {
    marginTop: Platform.OS === "android" ? StatusBar.currentHeight : 0
};

export const SignedOut = StackNavigator({
    // SignUp: {
    //   screen: SignUp,
    //   navigationOptions: {
    //     title: "Sign Up",
    //     headerStyle
    //   }
    // },
    SignIn: {
        screen: SignIn,
        navigationOptions: {
            title: "Sign In",
            headerStyle,
        }
    }
});

export const SignedIn = TabNavigator(
    {
        // Home: {
        //   screen: Home,
        //   navigationOptions: {
        //     tabBarLabel: "Home",
        //     // tabBarIcon: ({ tintColor }) =>
        //     //   <FontAwesome name="home" size={30} color={tintColor} />
        //   }
        // },
        //   Profile: {
        //     screen: Profile,
        //     navigationOptions: {
        //       tabBarLabel: "Profile",
        //       // tabBarIcon: ({ tintColor }) =>
        //       //   <FontAwesome name="user" size={30} color={tintColor} />
        //     }
        //   }
    },
    {
        tabBarOptions: {
            style: {
                paddingTop: Platform.OS === "android" ? StatusBar.currentHeight : 0
            }
        }
    }
);

export function createRootNavigator (signedIn) {
    return StackNavigator(
        {
            SignedIn: {
                screen: SignedIn,
                navigationOptions: { gesturesEnabled: false }
            },
            SignedOut: {
                screen: SignedOut,
                navigationOptions: { gesturesEnabled: false }
            }
        },
        {
            headerMode: "none",
            mode: "modal",
            initialRouteName: signedIn ? "SignedIn" : "SignedOut"
        }
    );
};
