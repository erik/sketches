import React from 'react';
import { StyleSheet, Text, View } from 'react-native';

import { isSignedIn } from './app/auth';
import { createRootNavigator } from './app/router';


export default class App extends React.Component {
    constructor (props) {
        super(props);

        this.state = {
            apiKey: null,
            signedIn: false,
            checkedSignIn: false
        };
    }

    componentWillMount () {
        isSignedIn()
            .then(state => {
                this.setState({...state, checkedSignIn: true});
            })
            .catch(err => {
                alert("Something went wrong! " + err);
            });
    }

    render() {
        // Don't do anything until we know if we're signed in.
        if (!this.state.checkedSignIn) {
            return null;
        }

        const Layout = createRootNavigator(this.state.signedIn, this.state.apiKey);
        return <Layout />;
    }
}

const styles = StyleSheet.create({
    container: {
        flex: 1,
        backgroundColor: '#fff',
        alignItems: 'center',
        justifyContent: 'center',
    },
});
