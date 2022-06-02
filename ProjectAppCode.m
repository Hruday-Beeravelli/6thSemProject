classdef Hruday2019BEE10_exported < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        UIFigure                  matlab.ui.Figure
        cpj                       matlab.ui.control.NumericEditField
        jEditFieldLabel_4         matlab.ui.control.Label
        dpj                       matlab.ui.control.NumericEditField
        jEditField_2Label_4       matlab.ui.control.Label
        dp                        matlab.ui.control.NumericEditField
        DLabel                    matlab.ui.control.Label
        cp                        matlab.ui.control.NumericEditField
        CLabel                    matlab.ui.control.Label
        apj                       matlab.ui.control.NumericEditField
        jEditFieldLabel_3         matlab.ui.control.Label
        bpj                       matlab.ui.control.NumericEditField
        jEditField_2Label_3       matlab.ui.control.Label
        bp                        matlab.ui.control.NumericEditField
        BLabel                    matlab.ui.control.Label
        ap                        matlab.ui.control.NumericEditField
        ALabel                    matlab.ui.control.Label
        sp                        matlab.ui.control.NumericEditField
        RegLabel_2                matlab.ui.control.Label
        reg                       matlab.ui.control.NumericEditField
        RegLabel                  matlab.ui.control.Label
        e                         matlab.ui.control.NumericEditField
        EffLabel                  matlab.ui.control.Label
        vsj                       matlab.ui.control.NumericEditField
        jEditField_2Label_2       matlab.ui.control.Label
        vs                        matlab.ui.control.NumericEditField
        VsLabel                   matlab.ui.control.Label
        isj                       matlab.ui.control.NumericEditField
        jEditFieldLabel_2         matlab.ui.control.Label
        is                        matlab.ui.control.NumericEditField
        IsLabel                   matlab.ui.control.Label
        Xcu_2                     matlab.ui.control.DropDown
        Xlu_2                     matlab.ui.control.DropDown
        PF                        matlab.ui.control.NumericEditField
        Zu                        matlab.ui.control.DropDown
        Bu                        matlab.ui.control.DropDown
        Ru                        matlab.ui.control.DropDown
        Xcu                       matlab.ui.control.DropDown
        Yu                        matlab.ui.control.DropDown
        fu                        matlab.ui.control.DropDown
        freq                      matlab.ui.control.NumericEditField
        FrequencyfEditFieldLabel  matlab.ui.control.Label
        ResetButton               matlab.ui.control.Button
        CalculateButton           matlab.ui.control.Button
        Length                    matlab.ui.control.NumericEditField
        LengthEditFieldLabel      matlab.ui.control.Label
        Lengu                     matlab.ui.control.DropDown
        Xu                        matlab.ui.control.DropDown
        Yj                        matlab.ui.control.NumericEditField
        jEditField_7Label         matlab.ui.control.Label
        Y                         matlab.ui.control.NumericEditField
        YEditFieldLabel           matlab.ui.control.Label
        Zj                        matlab.ui.control.NumericEditField
        jEditField_6Label         matlab.ui.control.Label
        Cu                        matlab.ui.control.DropDown
        Z                         matlab.ui.control.NumericEditField
        ZEditFieldLabel           matlab.ui.control.Label
        Bj                        matlab.ui.control.NumericEditField
        jEditField_5Label         matlab.ui.control.Label
        B                         matlab.ui.control.NumericEditField
        BEditFieldLabel           matlab.ui.control.Label
        Gj                        matlab.ui.control.NumericEditField
        jEditField_4Label         matlab.ui.control.Label
        G                         matlab.ui.control.NumericEditField
        GEditFieldLabel           matlab.ui.control.Label
        Xj                        matlab.ui.control.NumericEditField
        jEditField_3Label         matlab.ui.control.Label
        X                         matlab.ui.control.NumericEditField
        XEditFieldLabel           matlab.ui.control.Label
        Gu                        matlab.ui.control.DropDown
        Xcj                       matlab.ui.control.NumericEditField
        jEditField_2Label         matlab.ui.control.Label
        Xc                        matlab.ui.control.NumericEditField
        XCEditFieldLabel          matlab.ui.control.Label
        Xlu                       matlab.ui.control.DropDown
        Xlj                       matlab.ui.control.NumericEditField
        jEditFieldLabel           matlab.ui.control.Label
        Xl                        matlab.ui.control.NumericEditField
        XLEditFieldLabel          matlab.ui.control.Label
        Vr                        matlab.ui.control.NumericEditField
        ReceivingVoltageLabel     matlab.ui.control.Label
        Vru                       matlab.ui.control.DropDown
        P                         matlab.ui.control.NumericEditField
        PowerEditFieldLabel       matlab.ui.control.Label
        Pu                        matlab.ui.control.DropDown
        PFEditFieldLabel          matlab.ui.control.Label
        C                         matlab.ui.control.NumericEditField
        CEditFieldLabel           matlab.ui.control.Label
        L                         matlab.ui.control.NumericEditField
        LEditFieldLabel           matlab.ui.control.Label
        Lu                        matlab.ui.control.DropDown
        phaseSwitch               matlab.ui.control.Switch
        phaseSwitchLabel          matlab.ui.control.Label
        R                         matlab.ui.control.NumericEditField
        REditFieldLabel           matlab.ui.control.Label
    end

    
    methods (Access = public)
        
        function results = conver(app, Original, le,Olen)
            u = ["M","KM","Ω","mΩ","kΩ","Si","Hz","KHz","pF","nF","F","μF","V","kV"];
            up = ["Ω/KM","mΩ/KM","kΩ/KM","Si/KM","pF/KM","nF/KM","F/KM","mH/KM","μF/KM"];
            upp = ["V/Phase","kV/Phase"];
            if(ismember(le,up))
                results = Original*Olen;
                if(ismember(le,"kΩ/KM"))
                    results = results*1000;
                elseif(ismember(le,["mΩ/KM","mH/KM"]))
                    results = results*0.01;
                elseif(ismember(le,"nF/KM"))
                    results = results*10e-9;
                elseif(ismember(le,"pF/KM"))
                    results = results*10e-12;
                elseif(ismember(le,"μF/KM"))
                    results = results*10e-6;
                end
            else
                results = Original;
            end
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Button pushed function: CalculateButton
        function CalculateButtonPushed(app, event)
            u = ["M","KM","Ω","mΩ","kΩ","Si","Hz","KHz","pF","nF","F","μF","V","kV"];
            up = ["Ω/KM","mΩ/KM","kΩ/KM","Si/KM","pF/KM","nF/KM","F/KM","μF/KM"];
            upp = ["V/Phase","kV/Phase"];
            pf = app.PF.Value;
            len = app.Length.Value;
            p = app.P.Value;
            r = app.R.Value;
            vr = app.Vr.Value;
            f = app.freq.Value;
            l = app.L.Value;
            c = app.C.Value;
            xl = (app.Xl.Value + 1i*app.Xlj.Value);
            xc = (app.Xc.Value+ 1i*app.Xcj.Value);
            x =  (app.X.Value+ 1i*app.Xj.Value);
            g = (app.G.Value+ 1i*app.Gj.Value);
            b = (app.B.Value+ 1i*app.Bj.Value);
            z = (app.Z.Value+ 1i*app.Zj.Value);
            y = (app.Y.Value+ 1i*app.Yj.Value);
            ph = app.phaseSwitch.Value;
            g
            b
            l
            vr
            p
            theta = acos(pf);
            %if(ismember(app.Pu.Value,"MVA"))
                %p = p*1000;
            %end
            p
            vr = conver(app,vr,app.Vru.Value,len);
            r = conver(app,r,app.Ru.Value,len);
            f = conver(app,f,app.fu.Value,len);
            l = conver(app,l,app.Lu.Value,len);
            c = conver(app,c,app.Cu.Value,len);
            xl = conver(app,xl,app.Xlu.Value,len);
            xc = conver(app,xc,app.Xcu.Value,len);
            x = conver(app,x,app.Xu.Value,len);
            b = conver(app,b,app.Bu.Value,len);
            z = conver(app,z,app.Zu.Value,len);
            y
            y = conver(app,y,app.Yu.Value,len);
            y
            g = conver(app,g,app.Gu.Value,len);
            p
            vr
            r
            f
            l
            c
            x
            b
            z
            y

            if(abs(z) ==0)
                if(abs(x) ==0)
                    if(abs(xl)==0 || abs(xc)==0)
                        Z = r + 1i*6.14*f*l;
                    else
                        Z = r + 1i*(xl-xc)
                    
                else
                    Z = r + 1i*(x);
                end
            else
                Z = z;
            end
            Z
            if(abs(y) ==0)
                if(abs(g) ==0 && abs(b) ==0)
                    Y = 1i*6.14*f*c;
                else
                    Y = g + 1i*(b);
                end
            else
                Y = y;
            end
            Z
            Y

            A=1+(Y*Z)/2;
            B=Z*(1+(Y*Z)/4);
            C=Y;
            D=1+(Y*Z)/2;

            A
            B
            C
            D
            
            ir = (p/(sqrt(ph)*vr*pf))*cos(theta) - 1i*(p/(sqrt(ph)*vr*pf))*sin(theta);
            if(ismember(app.Vru.Value,"V")||ismember(app.Vru.Value,"kV"))
                vr = vr/sqrt(ph);
            end
            vs = A*vr + B*ir;
            is = C*vr + D*ir;
            vr
            ir
            
            RP=real(vr*conj(ir));
            SP=real(vs*conj(is));
            Eff=(RP/SP)*100;
            VR0=vs/A;
            Reg=((abs(VR0)-abs(vr))/abs(vr))*100;
            
            app.e.Value =Eff;
            app.ap.Value = real(A);
            app.apj.Value = imag(A);
            app.bp.Value = real(B);
            app.bpj.Value = imag(B);
            app.cp.Value = real(C);
            app.cpj.Value = imag(C);
            app.dp.Value = real(D);
            app.dpj.Value = imag(D);
            app.is.Value = real(is);
            app.vs.Value = real(vs);
            app.reg.Value = real(Reg);
            app.isj.Value = imag(is);
            app.vsj.Value = imag(vs);
            app.sp.Value = SP;
        end

        % Value changed function: Z
        function ZValueChanged(app, event)

        end

        % Button pushed function: ResetButton
        function ResetButtonPushed(app, event)
            %pf = app.PF.Value;
            %len = app.Length.Value;
            %p = app.P.Value;
            %r = app.R.Value;
            %vr = app.Vr.Value;
            %f = app.freq.Value;
            %l = app.L.Value;
            %c = app.C.Value;
            %xl = (app.Xl.Value + 1i*app.Xlj.Value);
            %xc = (app.Xc.Value+ 1i*app.Xcj.Value);
            %x =  (app.X.Value+ 1i*app.Xj.Value);
            %g = (app.G.Value+ 1i*app.Gj.Value);
            %b = (app.B.Value+ 1i*app.Bj.Value);
            %z = (app.Z.Value+ 1i*app.Zj.Value);
            %y = (app.Y.Value+ 1i*app.Yj.Value);
            %ph = app.phaseSwitch.Value;

        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create UIFigure and hide until all components are created
            app.UIFigure = uifigure('Visible', 'off');
            app.UIFigure.Position = [100 100 616 538];
            app.UIFigure.Name = 'MATLAB App';

            % Create REditFieldLabel
            app.REditFieldLabel = uilabel(app.UIFigure);
            app.REditFieldLabel.HorizontalAlignment = 'right';
            app.REditFieldLabel.Position = [25 403 25 22];
            app.REditFieldLabel.Text = 'R';

            % Create R
            app.R = uieditfield(app.UIFigure, 'numeric');
            app.R.HorizontalAlignment = 'left';
            app.R.Position = [65 403 55 22];

            % Create phaseSwitchLabel
            app.phaseSwitchLabel = uilabel(app.UIFigure);
            app.phaseSwitchLabel.HorizontalAlignment = 'center';
            app.phaseSwitchLabel.Position = [483 490 38 18];
            app.phaseSwitchLabel.Text = 'phase';

            % Create phaseSwitch
            app.phaseSwitch = uiswitch(app.UIFigure, 'slider');
            app.phaseSwitch.Items = {'1', '3'};
            app.phaseSwitch.ItemsData = [1 3];
            app.phaseSwitch.Position = [476 508 45 20];
            app.phaseSwitch.Value = 3;

            % Create Lu
            app.Lu = uidropdown(app.UIFigure);
            app.Lu.Items = {'mH', 'H', 'mH/KM', 'H/KM'};
            app.Lu.Position = [138 373 98 22];
            app.Lu.Value = 'mH';

            % Create LEditFieldLabel
            app.LEditFieldLabel = uilabel(app.UIFigure);
            app.LEditFieldLabel.HorizontalAlignment = 'right';
            app.LEditFieldLabel.Position = [25 373 25 22];
            app.LEditFieldLabel.Text = 'L';

            % Create L
            app.L = uieditfield(app.UIFigure, 'numeric');
            app.L.HorizontalAlignment = 'left';
            app.L.Position = [65 373 55 22];

            % Create CEditFieldLabel
            app.CEditFieldLabel = uilabel(app.UIFigure);
            app.CEditFieldLabel.HorizontalAlignment = 'right';
            app.CEditFieldLabel.Position = [310 373 25 22];
            app.CEditFieldLabel.Text = 'C';

            % Create C
            app.C = uieditfield(app.UIFigure, 'numeric');
            app.C.HorizontalAlignment = 'left';
            app.C.Position = [350 373 55 22];

            % Create PFEditFieldLabel
            app.PFEditFieldLabel = uilabel(app.UIFigure);
            app.PFEditFieldLabel.HorizontalAlignment = 'right';
            app.PFEditFieldLabel.Position = [25 487 25 22];
            app.PFEditFieldLabel.Text = 'PF';

            % Create Pu
            app.Pu = uidropdown(app.UIFigure);
            app.Pu.Items = {'KVA', 'MVA'};
            app.Pu.Position = [138 455 98 22];
            app.Pu.Value = 'MVA';

            % Create PowerEditFieldLabel
            app.PowerEditFieldLabel = uilabel(app.UIFigure);
            app.PowerEditFieldLabel.HorizontalAlignment = 'right';
            app.PowerEditFieldLabel.Position = [11 455 39 22];
            app.PowerEditFieldLabel.Text = 'Power';

            % Create P
            app.P = uieditfield(app.UIFigure, 'numeric');
            app.P.HorizontalAlignment = 'left';
            app.P.Position = [65 455 55 22];

            % Create Vru
            app.Vru = uidropdown(app.UIFigure);
            app.Vru.Items = {'V', 'kV', 'V/Phase', 'kV/Phase'};
            app.Vru.Position = [423 455 98 22];
            app.Vru.Value = 'kV';

            % Create ReceivingVoltageLabel
            app.ReceivingVoltageLabel = uilabel(app.UIFigure);
            app.ReceivingVoltageLabel.HorizontalAlignment = 'right';
            app.ReceivingVoltageLabel.Position = [277 447 58 30];
            app.ReceivingVoltageLabel.Text = {'Receiving'; 'Voltage'};

            % Create Vr
            app.Vr = uieditfield(app.UIFigure, 'numeric');
            app.Vr.HorizontalAlignment = 'left';
            app.Vr.Position = [350 455 55 22];

            % Create XLEditFieldLabel
            app.XLEditFieldLabel = uilabel(app.UIFigure);
            app.XLEditFieldLabel.HorizontalAlignment = 'right';
            app.XLEditFieldLabel.Position = [22 316 25 22];
            app.XLEditFieldLabel.Text = 'XL';

            % Create Xl
            app.Xl = uieditfield(app.UIFigure, 'numeric');
            app.Xl.Position = [56 316 61 22];

            % Create jEditFieldLabel
            app.jEditFieldLabel = uilabel(app.UIFigure);
            app.jEditFieldLabel.HorizontalAlignment = 'right';
            app.jEditFieldLabel.Position = [116 316 15 22];
            app.jEditFieldLabel.Text = '+ j';

            % Create Xlj
            app.Xlj = uieditfield(app.UIFigure, 'numeric');
            app.Xlj.Position = [135 316 61 22];

            % Create Xlu
            app.Xlu = uidropdown(app.UIFigure);
            app.Xlu.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Xlu.Position = [197 316 90 22];
            app.Xlu.Value = 'Ω';

            % Create XCEditFieldLabel
            app.XCEditFieldLabel = uilabel(app.UIFigure);
            app.XCEditFieldLabel.HorizontalAlignment = 'right';
            app.XCEditFieldLabel.Position = [317 317 25 22];
            app.XCEditFieldLabel.Text = 'XC';

            % Create Xc
            app.Xc = uieditfield(app.UIFigure, 'numeric');
            app.Xc.Position = [351 317 61 22];

            % Create jEditField_2Label
            app.jEditField_2Label = uilabel(app.UIFigure);
            app.jEditField_2Label.HorizontalAlignment = 'right';
            app.jEditField_2Label.Position = [411 317 15 22];
            app.jEditField_2Label.Text = '+ j';

            % Create Xcj
            app.Xcj = uieditfield(app.UIFigure, 'numeric');
            app.Xcj.Position = [430 317 61 22];

            % Create Gu
            app.Gu = uidropdown(app.UIFigure);
            app.Gu.Items = {'Si', 'Si/KM'};
            app.Gu.Position = [500 278 90 22];
            app.Gu.Value = 'Si';

            % Create XEditFieldLabel
            app.XEditFieldLabel = uilabel(app.UIFigure);
            app.XEditFieldLabel.HorizontalAlignment = 'right';
            app.XEditFieldLabel.Position = [22 278 25 22];
            app.XEditFieldLabel.Text = 'X';

            % Create X
            app.X = uieditfield(app.UIFigure, 'numeric');
            app.X.Position = [56 278 61 22];

            % Create jEditField_3Label
            app.jEditField_3Label = uilabel(app.UIFigure);
            app.jEditField_3Label.HorizontalAlignment = 'right';
            app.jEditField_3Label.Position = [116 278 15 22];
            app.jEditField_3Label.Text = '+ j';

            % Create Xj
            app.Xj = uieditfield(app.UIFigure, 'numeric');
            app.Xj.Position = [135 278 61 22];

            % Create GEditFieldLabel
            app.GEditFieldLabel = uilabel(app.UIFigure);
            app.GEditFieldLabel.HorizontalAlignment = 'right';
            app.GEditFieldLabel.Position = [317 278 25 22];
            app.GEditFieldLabel.Text = 'G';

            % Create G
            app.G = uieditfield(app.UIFigure, 'numeric');
            app.G.Position = [351 278 61 22];

            % Create jEditField_4Label
            app.jEditField_4Label = uilabel(app.UIFigure);
            app.jEditField_4Label.HorizontalAlignment = 'right';
            app.jEditField_4Label.Position = [411 279 15 22];
            app.jEditField_4Label.Text = '+ j';

            % Create Gj
            app.Gj = uieditfield(app.UIFigure, 'numeric');
            app.Gj.Position = [430 279 61 22];

            % Create BEditFieldLabel
            app.BEditFieldLabel = uilabel(app.UIFigure);
            app.BEditFieldLabel.HorizontalAlignment = 'right';
            app.BEditFieldLabel.Position = [22 243 25 22];
            app.BEditFieldLabel.Text = 'B';

            % Create B
            app.B = uieditfield(app.UIFigure, 'numeric');
            app.B.Position = [56 243 61 22];

            % Create jEditField_5Label
            app.jEditField_5Label = uilabel(app.UIFigure);
            app.jEditField_5Label.HorizontalAlignment = 'right';
            app.jEditField_5Label.Position = [116 243 15 22];
            app.jEditField_5Label.Text = '+ j';

            % Create Bj
            app.Bj = uieditfield(app.UIFigure, 'numeric');
            app.Bj.Position = [135 243 61 22];

            % Create ZEditFieldLabel
            app.ZEditFieldLabel = uilabel(app.UIFigure);
            app.ZEditFieldLabel.HorizontalAlignment = 'right';
            app.ZEditFieldLabel.Position = [22 199 25 22];
            app.ZEditFieldLabel.Text = 'Z';

            % Create Z
            app.Z = uieditfield(app.UIFigure, 'numeric');
            app.Z.ValueChangedFcn = createCallbackFcn(app, @ZValueChanged, true);
            app.Z.Position = [56 199 61 22];

            % Create Cu
            app.Cu = uidropdown(app.UIFigure);
            app.Cu.Items = {'pF', 'nF', 'μF', 'F', 'pF/KM', 'nF/KM', 'μF/KM', 'F/KM'};
            app.Cu.Position = [423 373 98 22];
            app.Cu.Value = 'μF/KM';

            % Create jEditField_6Label
            app.jEditField_6Label = uilabel(app.UIFigure);
            app.jEditField_6Label.HorizontalAlignment = 'right';
            app.jEditField_6Label.Position = [116 198 15 22];
            app.jEditField_6Label.Text = '+ j';

            % Create Zj
            app.Zj = uieditfield(app.UIFigure, 'numeric');
            app.Zj.Position = [135 198 61 22];

            % Create YEditFieldLabel
            app.YEditFieldLabel = uilabel(app.UIFigure);
            app.YEditFieldLabel.HorizontalAlignment = 'right';
            app.YEditFieldLabel.Position = [317 199 25 22];
            app.YEditFieldLabel.Text = 'Y';

            % Create Y
            app.Y = uieditfield(app.UIFigure, 'numeric');
            app.Y.Position = [351 199 61 22];

            % Create jEditField_7Label
            app.jEditField_7Label = uilabel(app.UIFigure);
            app.jEditField_7Label.HorizontalAlignment = 'right';
            app.jEditField_7Label.Position = [411 199 15 22];
            app.jEditField_7Label.Text = '+ j';

            % Create Yj
            app.Yj = uieditfield(app.UIFigure, 'numeric');
            app.Yj.Position = [430 199 61 22];

            % Create Xu
            app.Xu = uidropdown(app.UIFigure);
            app.Xu.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Xu.Position = [197 278 90 22];
            app.Xu.Value = 'Ω';

            % Create Lengu
            app.Lengu = uidropdown(app.UIFigure);
            app.Lengu.Items = {'M', 'KM'};
            app.Lengu.Position = [286 489 98 22];
            app.Lengu.Value = 'KM';

            % Create LengthEditFieldLabel
            app.LengthEditFieldLabel = uilabel(app.UIFigure);
            app.LengthEditFieldLabel.HorizontalAlignment = 'right';
            app.LengthEditFieldLabel.Position = [156 489 42 22];
            app.LengthEditFieldLabel.Text = 'Length';

            % Create Length
            app.Length = uieditfield(app.UIFigure, 'numeric');
            app.Length.HorizontalAlignment = 'left';
            app.Length.Position = [213 489 55 22];

            % Create CalculateButton
            app.CalculateButton = uibutton(app.UIFigure, 'push');
            app.CalculateButton.ButtonPushedFcn = createCallbackFcn(app, @CalculateButtonPushed, true);
            app.CalculateButton.Position = [201 154 100 23];
            app.CalculateButton.Text = 'Calculate';

            % Create ResetButton
            app.ResetButton = uibutton(app.UIFigure, 'push');
            app.ResetButton.ButtonPushedFcn = createCallbackFcn(app, @ResetButtonPushed, true);
            app.ResetButton.Position = [356 154 100 23];
            app.ResetButton.Text = 'Reset';

            % Create FrequencyfEditFieldLabel
            app.FrequencyfEditFieldLabel = uilabel(app.UIFigure);
            app.FrequencyfEditFieldLabel.HorizontalAlignment = 'right';
            app.FrequencyfEditFieldLabel.Position = [263 403 72 22];
            app.FrequencyfEditFieldLabel.Text = 'Frequency  f';

            % Create freq
            app.freq = uieditfield(app.UIFigure, 'numeric');
            app.freq.Position = [350 403 55 22];

            % Create fu
            app.fu = uidropdown(app.UIFigure);
            app.fu.Items = {'Hz', 'KHz'};
            app.fu.Placeholder = '0,0';
            app.fu.Position = [423 403 98 22];
            app.fu.Value = 'Hz';

            % Create Yu
            app.Yu = uidropdown(app.UIFigure);
            app.Yu.Items = {'Si', 'Si/KM'};
            app.Yu.Placeholder = '0,1';
            app.Yu.Position = [500 198 90 22];
            app.Yu.Value = 'Si';

            % Create Xcu
            app.Xcu = uidropdown(app.UIFigure);
            app.Xcu.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Xcu.Placeholder = '0,0,0,1,1,1';
            app.Xcu.Position = [500 316 90 22];
            app.Xcu.Value = 'Ω';

            % Create Ru
            app.Ru = uidropdown(app.UIFigure);
            app.Ru.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Ru.Placeholder = '0,0,0,1,1,1';
            app.Ru.Position = [138 403 90 22];
            app.Ru.Value = 'Ω/KM';

            % Create Bu
            app.Bu = uidropdown(app.UIFigure);
            app.Bu.Items = {'Si', 'Si/KM'};
            app.Bu.Placeholder = '0,1';
            app.Bu.Position = [197 243 90 22];
            app.Bu.Value = 'Si';

            % Create Zu
            app.Zu = uidropdown(app.UIFigure);
            app.Zu.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Zu.Position = [197 198 90 22];
            app.Zu.Value = 'Ω';

            % Create PF
            app.PF = uieditfield(app.UIFigure, 'numeric');
            app.PF.HorizontalAlignment = 'left';
            app.PF.Position = [65 487 55 22];

            % Create Xlu_2
            app.Xlu_2 = uidropdown(app.UIFigure);
            app.Xlu_2.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Xlu_2.Position = [199 47 90 22];
            app.Xlu_2.Value = 'Ω';

            % Create Xcu_2
            app.Xcu_2 = uidropdown(app.UIFigure);
            app.Xcu_2.Items = {'mΩ', 'Ω', 'kΩ', 'mΩ/KM', 'Ω/KM', 'kΩ/KM'};
            app.Xcu_2.Placeholder = '0,0,0,1,1,1';
            app.Xcu_2.Position = [502 47 90 22];
            app.Xcu_2.Value = 'Ω';

            % Create IsLabel
            app.IsLabel = uilabel(app.UIFigure);
            app.IsLabel.HorizontalAlignment = 'right';
            app.IsLabel.Position = [24 48 25 22];
            app.IsLabel.Text = 'Is';

            % Create is
            app.is = uieditfield(app.UIFigure, 'numeric');
            app.is.Position = [56 48 61 22];

            % Create jEditFieldLabel_2
            app.jEditFieldLabel_2 = uilabel(app.UIFigure);
            app.jEditFieldLabel_2.HorizontalAlignment = 'right';
            app.jEditFieldLabel_2.Position = [118 47 15 22];
            app.jEditFieldLabel_2.Text = '+ j';

            % Create isj
            app.isj = uieditfield(app.UIFigure, 'numeric');
            app.isj.Position = [137 47 61 22];

            % Create VsLabel
            app.VsLabel = uilabel(app.UIFigure);
            app.VsLabel.HorizontalAlignment = 'right';
            app.VsLabel.Position = [319 48 25 22];
            app.VsLabel.Text = 'Vs';

            % Create vs
            app.vs = uieditfield(app.UIFigure, 'numeric');
            app.vs.Position = [353 48 61 22];

            % Create jEditField_2Label_2
            app.jEditField_2Label_2 = uilabel(app.UIFigure);
            app.jEditField_2Label_2.HorizontalAlignment = 'right';
            app.jEditField_2Label_2.Position = [413 48 15 22];
            app.jEditField_2Label_2.Text = '+ j';

            % Create vsj
            app.vsj = uieditfield(app.UIFigure, 'numeric');
            app.vsj.Position = [432 48 61 22];

            % Create EffLabel
            app.EffLabel = uilabel(app.UIFigure);
            app.EffLabel.HorizontalAlignment = 'right';
            app.EffLabel.Position = [24 9 25 22];
            app.EffLabel.Text = 'Eff';

            % Create e
            app.e = uieditfield(app.UIFigure, 'numeric');
            app.e.Position = [58 9 61 22];

            % Create RegLabel
            app.RegLabel = uilabel(app.UIFigure);
            app.RegLabel.HorizontalAlignment = 'right';
            app.RegLabel.Position = [227 9 27 22];
            app.RegLabel.Text = 'Reg';

            % Create reg
            app.reg = uieditfield(app.UIFigure, 'numeric');
            app.reg.Position = [263 9 61 22];

            % Create RegLabel_2
            app.RegLabel_2 = uilabel(app.UIFigure);
            app.RegLabel_2.HorizontalAlignment = 'right';
            app.RegLabel_2.Position = [443 10 25 22];
            app.RegLabel_2.Text = 'SP';

            % Create sp
            app.sp = uieditfield(app.UIFigure, 'numeric');
            app.sp.Position = [477 10 61 22];

            % Create ALabel
            app.ALabel = uilabel(app.UIFigure);
            app.ALabel.HorizontalAlignment = 'right';
            app.ALabel.Position = [31 118 25 22];
            app.ALabel.Text = 'A';

            % Create ap
            app.ap = uieditfield(app.UIFigure, 'numeric');
            app.ap.Position = [65 118 61 22];

            % Create BLabel
            app.BLabel = uilabel(app.UIFigure);
            app.BLabel.HorizontalAlignment = 'right';
            app.BLabel.Position = [326 119 25 22];
            app.BLabel.Text = 'B';

            % Create bp
            app.bp = uieditfield(app.UIFigure, 'numeric');
            app.bp.Position = [360 119 61 22];

            % Create jEditField_2Label_3
            app.jEditField_2Label_3 = uilabel(app.UIFigure);
            app.jEditField_2Label_3.HorizontalAlignment = 'right';
            app.jEditField_2Label_3.Position = [420 119 15 22];
            app.jEditField_2Label_3.Text = '+ j';

            % Create bpj
            app.bpj = uieditfield(app.UIFigure, 'numeric');
            app.bpj.Position = [439 119 61 22];

            % Create jEditFieldLabel_3
            app.jEditFieldLabel_3 = uilabel(app.UIFigure);
            app.jEditFieldLabel_3.HorizontalAlignment = 'right';
            app.jEditFieldLabel_3.Position = [125 118 15 22];
            app.jEditFieldLabel_3.Text = '+ j';

            % Create apj
            app.apj = uieditfield(app.UIFigure, 'numeric');
            app.apj.Position = [144 118 61 22];

            % Create CLabel
            app.CLabel = uilabel(app.UIFigure);
            app.CLabel.HorizontalAlignment = 'right';
            app.CLabel.Position = [33 89 25 22];
            app.CLabel.Text = 'C';

            % Create cp
            app.cp = uieditfield(app.UIFigure, 'numeric');
            app.cp.Position = [67 89 61 22];

            % Create DLabel
            app.DLabel = uilabel(app.UIFigure);
            app.DLabel.HorizontalAlignment = 'right';
            app.DLabel.Position = [328 90 25 22];
            app.DLabel.Text = 'D';

            % Create dp
            app.dp = uieditfield(app.UIFigure, 'numeric');
            app.dp.Position = [362 90 61 22];

            % Create jEditField_2Label_4
            app.jEditField_2Label_4 = uilabel(app.UIFigure);
            app.jEditField_2Label_4.HorizontalAlignment = 'right';
            app.jEditField_2Label_4.Position = [422 90 15 22];
            app.jEditField_2Label_4.Text = '+ j';

            % Create dpj
            app.dpj = uieditfield(app.UIFigure, 'numeric');
            app.dpj.Position = [441 90 61 22];

            % Create jEditFieldLabel_4
            app.jEditFieldLabel_4 = uilabel(app.UIFigure);
            app.jEditFieldLabel_4.HorizontalAlignment = 'right';
            app.jEditFieldLabel_4.Position = [127 89 15 22];
            app.jEditFieldLabel_4.Text = '+ j';

            % Create cpj
            app.cpj = uieditfield(app.UIFigure, 'numeric');
            app.cpj.Position = [146 89 61 22];

            % Show the figure after all components are created
            app.UIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = Hruday2019BEE10_exported

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.UIFigure)

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.UIFigure)
        end
    end
end
