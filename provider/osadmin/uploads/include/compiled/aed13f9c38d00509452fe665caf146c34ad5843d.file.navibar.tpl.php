<?php /* Smarty version Smarty-3.1.15, created on 2014-11-13 13:00:54
         compiled from "E:\software\wamp\www\osadmin\uploads\include\template\navibar.tpl" */ ?>
<?php /*%%SmartyHeaderCode:575754643b06d441c2-28754615%%*/if(!defined('SMARTY_DIR')) exit('no direct access allowed');
$_valid = $_smarty_tpl->decodeProperties(array (
  'file_dependency' => 
  array (
    'aed13f9c38d00509452fe665caf146c34ad5843d' => 
    array (
      0 => 'E:\\software\\wamp\\www\\osadmin\\uploads\\include\\template\\navibar.tpl',
      1 => 1402209212,
      2 => 'file',
    ),
  ),
  'nocache_hash' => '575754643b06d441c2-28754615',
  'function' => 
  array (
  ),
  'variables' => 
  array (
    'sidebarStatus' => 0,
    'user_info' => 0,
    'osa_templates' => 0,
    'key' => 0,
    'name' => 0,
  ),
  'has_nocache_code' => false,
  'version' => 'Smarty-3.1.15',
  'unifunc' => 'content_54643b06e076f5_14809995',
),false); /*/%%SmartyHeaderCode%%*/?>
<?php if ($_valid && !is_callable('content_54643b06e076f5_14809995')) {function content_54643b06e076f5_14809995($_smarty_tpl) {?><?php if ($_smarty_tpl->tpl_vars['sidebarStatus']->value=='yes') {?> 
  <body id="body" class="body">
  <?php } else { ?>
  <body id="body" class="body-fullscreen">
  <?php }?>
  <!--<![endif]-->
<div class="navbar">
        <div class="navbar-inner">
                <ul class="nav pull-right">
                    
					<?php if ($_smarty_tpl->tpl_vars['sidebarStatus']->value=='yes') {?>
						<li class="doSidebarClz"><a href="#" class="hidden-phone visible-tablet visible-desktop" role="button">
						关闭侧栏<i class="icon-step-backward"></i>
						</a></li>
					<?php } else { ?>
						<li class="doSidebarClz"><a href="#" class="hidden-phone visible-tablet visible-desktop" role="button">
						打开侧栏<i class="icon-step-forward"></i>
						</a></li>
					<?php }?>
					 
					<?php if ($_smarty_tpl->tpl_vars['user_info']->value['setting']) {?>
                    <li id="fat-menu" class="dropdown">
                        <a href="#" role="button" class="dropdown-toggle" data-toggle="dropdown">
							<i class="icon-cog"></i>设置<i class="icon-caret-down"></i>
						</a>
                        <ul class="dropdown-menu">
                            <li><a href="<?php echo @constant('ADMIN_URL');?>
/panel/setting.php">系统设置</a></li>
                        </ul>
                    </li>
					<?php }?>
					
					<li id="fat-menu" class="dropdown">
                        <a href="#" role="button" class="dropdown-toggle" data-toggle="dropdown">
							
                            选择模板
                            <i class="icon-caret-down"></i>
                        </a>

                        <ul class="dropdown-menu">
							<?php  $_smarty_tpl->tpl_vars['name'] = new Smarty_Variable; $_smarty_tpl->tpl_vars['name']->_loop = false;
 $_smarty_tpl->tpl_vars['key'] = new Smarty_Variable;
 $_from = $_smarty_tpl->tpl_vars['osa_templates']->value; if (!is_array($_from) && !is_object($_from)) { settype($_from, 'array');}
foreach ($_from as $_smarty_tpl->tpl_vars['name']->key => $_smarty_tpl->tpl_vars['name']->value) {
$_smarty_tpl->tpl_vars['name']->_loop = true;
 $_smarty_tpl->tpl_vars['key']->value = $_smarty_tpl->tpl_vars['name']->key;
?>
                            <li><a href="<?php echo @constant('ADMIN_URL');?>
/panel/set.php?t=<?php echo $_smarty_tpl->tpl_vars['key']->value;?>
"><?php echo $_smarty_tpl->tpl_vars['name']->value;?>
</a></li>
							<?php } ?>
                        </ul>
                    </li>
					
					<li id="fat-menu" class="dropdown">
                        <a href="#" role="button" class="dropdown-toggle" data-toggle="dropdown">
                            <i class="icon-user"></i> <?php echo $_smarty_tpl->tpl_vars['user_info']->value['user_name'];?>

                            <i class="icon-caret-down"></i>
                        </a>

                        <ul class="dropdown-menu">
                            <li><a tabindex="-1" href="<?php echo @constant('ADMIN_URL');?>
/panel/profile.php">我的账号</a></li>
                            <li><a tabindex="-1" href="<?php echo @constant('ADMIN_URL');?>
/panel/logout.php">登出</a></li>
                        </ul>
                    </li>
                    
                </ul>
                <a class="brand" href="<?php echo @constant('ADMIN_URL');?>
/panel/index.php"><span class="first"></span> <span class="second"><?php echo @constant('COMPANY_NAME');?>
</span></a>
        </div>
</div><?php }} ?>
